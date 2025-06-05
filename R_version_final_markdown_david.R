#' ---
#' title: "Análisis de Series Temporales: Gastos en Alimentación en Australia"
#' author: "Tu Nombre"
#' date: "`r Sys.Date()`"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     theme: journal
#'     highlight: tango
#' ---
#' 
#' ## Carga de paquetes y datos

library(forecast)
source("BoxCoxTransformation.R")
require(astsa)         # load astsa
source("Diagnostic.R")  #(load the functions in "diagnostic")
require(portes)       # load portes  (Ljung-Box test, McLeod-Li_83 test)
# require(Nortes)

#' Importamos los datos de la serie temporal
data = read.table("data9.txt", header = TRUE)

#' ## Análisis Exploratorio (EDA)

#' Creamos el objeto de serie temporal
z <- ts(data[,1], start = c(1980, 4), frequency = 12)

#' ### Visualización de la serie original
ts.plot(z, main = "Total monthly expenditure on cafes, restaurants and takeaway food services in Australia, April 1980 - April 2015", 
        ylab = "Consumption (in billions of dollars)")

#' ### Gráfico estacional
ggseasonplot(z, main = "Seasonal plot: Total monthly expenditure on cafes, restaurants and takeaway food")

#' **Observaciones iniciales:**
#' - Presencia de componente de tendencia creciente
#' - Estacionalidad anual evidente (s=12)
#' - Variabilidad no constante (heterocedasticidad)

#' ## Transformación Box-Cox
Plot.var(z, 12)
BoxCox(z, 12)

#' **Resultado de la transformación:**
#' - Lambda óptimo = 0.2
#' - No es necesario usar transformación logarítmica
X.tilde = (z**0.2 - 1)/0.2
ts.plot(X.tilde, main = "Serie transformada con Box-Cox (λ=0.2)")

#' ### Función de Autocorrelación (ACF)
Acf(X.tilde, main = "ACF de la serie transformada", xlab = "Lag", ylab = "ACF")

#' ## Diferenciación para lograr estacionariedad

#' ### Primera diferencia regular
Wt.1 = diff(X.tilde, lag = 1, differences = 1)
ts.plot(as.numeric(Wt.1), xlab = "", ylab = "", 
        main = "Primera diferencia regular de la serie transformada")
ggseasonplot(Wt.1, main = "Gráfico estacional de la primera diferencia")

#' ### Primera diferencia estacional
Wt.2 = diff(Wt.1, lag = 12, differences = 1)
ts.plot(as.numeric(Wt.2), xlab = "", ylab = "", 
        main = "Primeras diferencias regular y estacional", type = "l")

#' ### ACF y PACF de la serie diferenciada
acf.2 = Acf(Wt.2, 80, main = "ACF: Diferencias regular y estacional", 
            xlab = "Lag", ylab = "ACF")
pacf.2 = Pacf(Wt.2, 80, main = "PACF: Diferencias regular y estacional", 
              xlab = "Lag", ylab = "PACF")

#' **Identificación de modelo:**
#' - Componente MA(1) regular
#' - Componente MA(1) o SMA(1) estacional
#' - Posible componente AR estacional

#' ## Ajuste de modelos SARIMA

mod1 = Arima(z, order = c(2,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod2 = Arima(z, order = c(2,1,1), seasonal = list(order = c(5,1,1), period = 12), lambda = 0.2)
mod3 = Arima(z, order = c(3,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod4 = Arima(z, order = c(0,1,1), seasonal = list(order = c(2,1,2), period = 12), lambda = 0.2)
mod5 = Arima(z, order = c(0,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod6 = Arima(z, order = c(0,1,1), seasonal = list(order = c(3,1,2), period = 12), lambda = 0.2)
mod7 = Arima(z, order = c(1,1,0), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)

modelos <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
names(modelos) <- paste0("mod", 1:7)

#' ## Comparación de modelos

#' Creación de tabla comparativa
tabla_resultados <- data.frame()
for (nombre in names(modelos)) {
  mod <- modelos[[nombre]]
  acc <- accuracy(mod)
  
  rmse_val <- acc["Training set", "RMSE"]
  
  tabla_resultados <- rbind(tabla_resultados, data.frame(
    Model = nombre,
    AIC = round(mod$aic, 2),
    AICc = round(mod$aicc, 2),
    BIC = round(mod$bic, 2),
    RMSE = round(rmse_val, 6),
    MAE = round(acc["Training set", "MAE"], 4)
  ))
}

#' ### Tabla comparativa de métricas
library(dplyr)
library(kableExtra)

best_AIC <- min(tabla_resultados$AIC)
best_AICc <- min(tabla_resultados$AICc)
best_BIC <- min(tabla_resultados$BIC)
best_RMSE <- min(tabla_resultados$RMSE)
best_MAE <- min(tabla_resultados$MAE)

#' ## Diagnóstico de residuos

#' ### Pruebas de Ljung-Box personalizadas
My.Ljung.Box.FixedK <- function(x, np, k) {
  n <- length(x)
  SampleACF <- as.numeric(unlist(Acf(x, lag.max = k, plot = FALSE)))[2:(k + 1)]
  Q.ML <- n * (n + 2) * sum((SampleACF^2) / (n - seq_len(k)))
  pval <- 1 - pchisq(Q.ML, df = k - np)
  data.frame(k = k, Test_Statistic = Q.ML, P_value = pval)
}

#' ### Pruebas no paramétricas
NonParametric.Tests(mod2$residuals)    
NonParametric.Tests(mod3$residuals)    
NonParametric.Tests(mod5$residuals)    

#' ### Pruebas de normalidad
Check.normality(mod2$residuals)
Check.normality(mod3$residuals)
Check.normality(mod5$residuals)

#' ## Validación y pronóstico

#' ### Serie observada vs ajustada (Modelo 3)
plot(seq(1,408), z, ylab = "Consumption (in billions of dollars)",
     xlab = "", type = "l", main = "Ajuste del Modelo 3")
points(seq(1,408), forecast(mod3)$fitted, col = "red", type = "l")
legend("topleft", legend = c("Observado", "Ajustado"), 
       col = c("black", "red"), lty = 1)

#' ### Pronóstico a 30 meses
Predic.mod = forecast(mod3, 30)
plot(Predic.mod, main = "Pronóstico de gastos a 30 meses")