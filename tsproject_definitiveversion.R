library(forecast)
source("BoxCoxTransformation.R")
require(astsa)         # load astsa
source("Diagnostic.R")  #(load the functions in "diagnostic")
require(portes)       # load portes  (Ljung-Box test, McLeod-Li_83 test)
# require(Nortes)
data=read.table("data9.txt",header=T)

#EDA
z <- ts(data[,1], start = c(1980, 4), frequency = 12)
ts.plot(z,main="Total monthly expenditure on cafes, restaurants and takeaway food services in Australia, April 1980 - April 2015" ,ylab="Consumption (in billions of dollars)")
ggseasonplot(z,main="Seasonal plot: Total monthly expenditure on cafes, restaurants and takeaway food")
#From this graphs, we can see that there is a trend component (the lines go higher as 
#time goes on) which means that the mean is not 0, there is a seasonal component s=12, and 
#the variance is not constant (lines become less straight at the top)

#Variance stabilizing transformation (Box-Cox) 
Plot.var(z,12)
#Clearly there is a linear relationship between the logarithm of the mean and the 
#logarithm of the standard deviation, so we can use the box-cox transformation
BoxCox(z,12)
#lambda=0.2 not close to zero, so it is not necessary to use the logarithmic transformation
X.tilde=(z**(0.2)-1)/0.2
mean(X.tilde)
ts.plot(X.tilde,)

Acf(X.tilde,main="ACF of the transformed series",xlab="Lag",ylab="ACF")
#We apply the differentiating operator because there is a geometrical decrease in the Acf:
Wt.1=diff(X.tilde,lag=1,differences=1) 
ts.plot(as.numeric(Wt.1),xlab="",ylab="",main="First regular difference of the transformed series") 
ggseasonplot(Wt.1, main="Seasonal plot of the first regular difference of the transformed series")
mean(Wt.1)
#Mean is pretty much 0, we can stop differentiating
Acf(Wt.1,60, main="ACF of the first regular difference of the transformed series",xlab="Lag",ylab="ACF")
#We can clearly see there is a seasonal component in the series,
#so we need to differentiate seasonally at least once
Wt.2=diff(Wt.1,lag=12,differences=1)
ts.plot(as.numeric(Wt.2),xlab="", ylab="",main="First regular and seasonal difference of the transformed series",type="l") 
mean(Wt.2)
#Mean is 0, there is stationarity

#Let's look at the Acf
acf.2=Acf(Wt.2,80, main="ACF of the first regular and seasonal difference of the transformed series",xlab="Lag",ylab="ACF")
points(seq(12,80,by=12), acf.2$acf[seq(13,81,by=12)],type="h",col="#52b516",lwd=1.5)
points(seq(1), acf.2$acf[seq(2,2)],type="h",col="red",lwd=1.5)
#Regular MA=1 since only first coefficient different from 0
#Seasonal MA=1 or 3 it depends if you count on lag=24 as 0 or not
#Slight geometrical decease, possible AR component in both regular and seasonal

#Let's look at the Pacf
pacf.2=Pacf(Wt.2,80, main="PACF of the first regular and seasonal difference of the transformed series",xlab="Lag",ylab="PACF")
points(seq(12,80,by=12), pacf.2$acf[seq(12,81,by=12)],type="h",col="#52b516",lwd=1.5)
points(seq(1), acf.2$acf[seq(2,2)],type="h",col="red",lwd=1.5)
#Regular AR=1
#Seasonal AR=2 or MA=2
#Clear geometrical decease, it's very likely there is a MA component in both regular and seasonal

#Based on this, we can consider the following models:
mod1=Arima(z,order=c(2,1,1),seasonal=list(order=c(3,1,1),period=12),lambda=0.2)
mod2=Arima(z,order=c(2,1,1),seasonal=list(order=c(5,1,1),period=12),lambda=0.2)
mod3=Arima(z,order=c(3,1,1),seasonal=list(order=c(3,1,1),period=12),lambda=0.2)
mod4=Arima(z,order=c(0,1,1),seasonal=list(order=c(2,1,2),period=12),lambda=0.2)
mod5=Arima(z,order=c(0,1,1),seasonal=list(order=c(3,1,1),period=12),lambda=0.2)
mod6=Arima(z,order=c(0,1,1),seasonal=list(order=c(3,1,2),period=12),lambda=0.2)
mod7=Arima(z,order=c(1,1,0),seasonal=list(order=c(3,1,1),period=12 ),lambda=0.2)

modelos <- list(
  mod1,
  mod2,
  mod3,
  mod4,
  mod5,
  mod6,
  mod7
)

# Métricas

# Crear dataframe con métricas
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

# Identificar los mejores valores
best_AIC <- min(tabla_resultados$AIC)
best_AICc <- min(tabla_resultados$AICc)
best_BIC <- min(tabla_resultados$BIC)
best_RMSE <- min(tabla_resultados$RMSE)
best_MAE <- min(tabla_resultados$MAE)

library(dplyr)
library(kableExtra)

# Definir colores
color_texto <- "#111111"  # Gris muy oscuro (casi negro)
color_resaltado <- "#90EE90"  # Verde claro para resaltar
color_encabezado <- "#3498db"  # Azul para encabezado

# Crear tabla con celdas resaltadas y texto oscuro
tabla_formateada <- tabla_resultados %>%
  mutate(
    AIC = cell_spec(AIC, "html", 
                    background = ifelse(AIC == best_AIC, color_resaltado, "white"),
                    color = color_texto),
    AICc = cell_spec(AICc, "html", 
                     background = ifelse(AICc == best_AICc, color_resaltado, "white"),
                     color = color_texto),
    BIC = cell_spec(BIC, "html", 
                    background = ifelse(BIC == best_BIC, color_resaltado, "white"),
                    color = color_texto),
    RMSE = cell_spec(RMSE, "html", 
                     background = ifelse(RMSE == best_RMSE, color_resaltado, "white"),
                     color = color_texto),
    MAE = cell_spec(MAE, "html", 
                    background = ifelse(MAE == best_MAE, color_resaltado, "white"),
                    color = color_texto)
  ) %>%
  kable(align = "c", escape = FALSE, format = "html",
        caption = "Monthly Expenditure on Food Services (Australia, Apr 1980–Apr 2015) - Model comparison") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, background = color_encabezado, color = "white") %>%
  column_spec(1, bold = TRUE, color = color_texto) %>%
  add_header_above(c(" " = 1, "Likelihood Criteria" = 3, "Error Metrics" = 2),
                   background = color_encabezado, color = "white") %>%
  footnote(general = "The RMSE represents the square root of the MSE<br>Green cells highlight optimal values (lowest for all metrics)",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)

# Guardar tabla
save_kable(tabla_formateada, "model_comparison_table.png")
save_kable(tabla_formateada, "model_comparison_table.html")
save_kable(tabla_formateada, "model_comparison_table.pdf")





My.Ljung.Box.FixedK <- function(x, np, k) {
  n <- length(x)
  
  # ACF estimada hasta lag k
  SampleACF <- as.numeric(unlist(Acf(x, lag.max = k, plot = FALSE)))[2:(k + 1)]
  
  # Estadístico de Ljung-Box
  Q.ML <- n * (n + 2) * sum((SampleACF^2) / (n - seq_len(k)))
  
  # P-valor usando distribución chi-cuadrado con (k - np) grados de libertad
  pval <- 1 - pchisq(Q.ML, df = k - np)
  
  # Resultado como data frame
  result <- data.frame(
    k = k,
    Test_Statistic = Q.ML,
    P_value = pval
  )
  
  return(result)
}


#Test the null hypothesis that the residuals are realizations of an IID noise process.
NonParametric.Tests(mod2$residuals)    
NonParametric.Tests(mod3$residuals)    
NonParametric.Tests(mod5$residuals)    

My.Ljung.Box.FixedK(mod2$residuals,np=10,k=24)
My.Ljung.Box.FixedK(mod3$residuals,np=9,k=24)
My.Ljung.Box.FixedK(mod5$residuals,np=6,k=24)


#Normality tests
Check.normality(mod2$residuals)
Check.normality(mod3$residuals)
Check.normality(mod5$residuals)

#Plot series vs fitted values
plot(seq(1,408), z,ylab="Consumption (in billions of dollars)",xlab="",type="l",main="Model 3")
points(seq(1,408),forecast(mod3)$fitted,col="red",type="l")


#Forecasting
Predic.mod=forecast(mod3,30)
plot(Predic.mod,20)

