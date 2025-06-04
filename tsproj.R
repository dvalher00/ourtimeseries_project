library(forecast)
source("BoxCoxTransformation.R")
require(astsa)
source("Diagnostic.R")
require(portes)

# Leer datos
data <- read.table("data9.txt", header = TRUE)
z <- ts(data[, 1], start = c(1980, 4), frequency = 12)

# 1. Análisis exploratorio - CORREGIDO: Usar plot correcto para series temporales
plot(z, xlab = "Months", ylab = "Millions of dollars",
     main = "Monthly expenditure on cafes and restaurants in Australia")

# Gráficos de diagnóstico
Acf(z, 40, main = "ACF of the time series")
ggseasonplot(z, main = "Seasonal plot")

# 2. Transformación y diferenciación - CORREGIDO: Títulos y tipos de gráfico
X_tilde <- log(z)
Wt_1 <- diff(X_tilde)
plot(Wt_1, main = "First difference of log series")
Acf(Wt_1, 80, main = "ACF after first difference")

# 3. Diferenciación estacional - CORREGIDO: Variable corregida
Wt_2 <- diff(Wt_1, lag = 12)
plot(Wt_2, main = "Seasonally differenced series")
Acf(Wt_2, 50, main = "ACF after seasonal differencing")
Pacf(Wt_2, 80, main = "PACF after seasonal differencing")

# 4. Modelado automático - CORREGIDO: Parámetro lambda=0 implica log-transform
mod_auto <- auto.arima(z, seasonal = TRUE, lambda = 0, 
                       stepwise = FALSE, approximation = FALSE)
summary(mod_auto)
checkresiduals(mod_auto)

# 5. Modelos manuales - CORREGIDO: Usar nombres consistentes
modelos <- list(
  mod1 = Arima(z, order = c(1,1,1), seasonal = c(3,1,1), lambda = 0),
  mod2 = Arima(z, order = c(1,1,1), seasonal = c(5,1,1), lambda = 0),
  mod3 = Arima(z, order = c(1,1,1), seasonal = c(3,1,3), lambda = 0),
  mod4 = Arima(z, order = c(1,1,1), seasonal = c(5,1,3), lambda = 0),
  mod5 = Arima(z, order = c(2,1,1), seasonal = c(3,1,1), lambda = 0),
  mod6 = Arima(z, order = c(2,1,1), seasonal = c(5,1,1), lambda = 0),
  mod7 = Arima(z, order = c(2,1,1), seasonal = c(3,1,3), lambda = 0),
  mod8 = Arima(z, order = c(2,1,1), seasonal = c(5,1,3), lambda = 0),
  mod9 = Arima(z, order = c(3,1,1), seasonal = c(3,1,1), lambda = 0),
  mod10 = Arima(z, order = c(3,1,1), seasonal = c(5,1,1), lambda = 0),
  mod11 = Arima(z, order = c(3,1,1), seasonal = c(3,1,3), lambda = 0),
  mod12 = Arima(z, order = c(3,1,1), seasonal = c(5,1,3), lambda = 0)
)

# 6. Validación cruzada - CORREGIDO: Inicialización correcta de variables
n.data <- length(z)
n.min <- 300  # Tamaño inicial del entrenamiento
n.forecasts <- n.data - n.min

# Inicializar matrices de resultados
MSE_matrix <- matrix(NA, nrow = n.forecasts, ncol = 8)
MAE_matrix <- matrix(NA, nrow = n.forecasts, ncol = 8)
model_cols <- 5:12  # Corresponden a mod5-mod12

for (k in 1:n.forecasts) {
  train_data <- window(z, end = c(1980, 3 + n.min + k - 1))
  
  for (i in 1:8) {
    model_index <- i + 4  # Modelos 5 a 12
    fit <- Arima(train_data, 
                 order = modelos[[model_index]]$arma[c(1,6,2)],
                 seasonal = list(order = modelos[[model_index]]$arma[c(3,7,4)]),
                 lambda = 0)
    
    forecast_val <- forecast(fit, h = 1)$mean
    actual_val <- z[n.min + k]
    
    MSE_matrix[k, i] <- (actual_val - forecast_val)^2
    MAE_matrix[k, i] <- abs(actual_val - forecast_val)
  }
}

# 7. Resultados - CORREGIDO: Cálculo correcto de métricas
results <- rbind(
  colMeans(MSE_matrix, na.rm = TRUE),
  colMeans(MAE_matrix, na.rm = TRUE)
)
colnames(results) <- paste("Model", 5:12)
rownames(results) <- c("MSE", "MAE")
print(results)

# 8. Selección de mejor modelo - CORREGIDO: Usar el modelo con menor MSE
best_model_index <- which.min(results[1, ]) + 4
best_model <- modelos[[best_model_index]]
summary(best_model)

# 9. Pronósticos - CORREGIDO: Usar el mejor modelo seleccionado
forecast_best <- forecast(best_model, h = 24)
plot(forecast_best, main = "24-month Forecast with Best Model")

# Gráfico comparativo últimos datos vs ajuste
plot(window(z, start = c(2010, 1)), 
     main = "Actual vs Fitted Values")
lines(fitted(best_model), col = "red")
legend("topleft", legend = c("Actual", "Fitted"), 
       col = c("black", "red"), lty = 1)

