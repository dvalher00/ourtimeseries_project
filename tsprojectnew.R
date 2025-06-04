library(forecast)
source("BoxCoxTransformation.R")
require(astsa)         # load astsa
source("Diagnostic.R")  #(load the functions in "diagnostic")
require(portes)       # load portes  (Ljung-Box test, McLeod-Li_83 test)
# require(Nortes
data=read.table("data9.txt",header=T)

plot.ts(data[,1],xlab="Months", ylab="dolars?",main="Total monthly expenditure on cafes, restaurants and takeaway food services in Australia, April 1980 - April 2015")
#Better:

z <- ts(data[,1], start = c(1980, 4), frequency = 12) 
ts.plot(z,main="Total monthly expenditure on cafes, restaurants and takeaway food services in Australia, April 1980 - April 2015" ,ylab="registrations")
Acf(z,40, main="ACF of the time series",xlab="Lag",ylab="ACF")
ggseasonplot(z,main="Seasonal plot: Total monthly expenditure on cafes, restaurants and takeaway food")

#Thing:
ts.plot(z[1:48],xlab="t", ylab="matriculaciones",main="First 48 values of the time series",type="b")  
points(seq(13,24),z[13:24],type="b",col="blue",lwd=1.4)
points(seq(25,36),z[25:36],type="b",col="red",lwd=1.4)
points(seq(37,48),z[37:48],type="b",col="green",lwd=1.4)

Plot.var(z,12)
BoxCox(z,12)

X.tilde=log(z)
Wt.1=diff(X.tilde,lag=1,differences=1) 
ts.plot(Wt.1,xlab="t", ylab="",main="First difference of the logarithm of the  registration series",type="l") 
ggseasonplot(Wt.1, main=" ")
Acf(Wt.1,80, main="ACF of the time series",xlab="Lag",ylab="ACF")
mean(Wt.1)

Wt.2=diff(Wt.1,lag=12,differences=1) 
ts.plot(Wt.1,xlab="t", ylab="",main="Second difference of the logarithm of the  registration series",type="l") 
ggseasonplot(Wt.2, main=" ")
mean(Wt.2)
Acf(Wt.2,50, main="ACF of the time series",xlab="Lag",ylab="ACF")
#Looking at the graph, seasonal MA 1 or 3, regular MA 1
Pacf(Wt.2,80, main="PACF of the time series",xlab="Lag",ylab="ACF")
#Seasonal AR 3 or 5, regular AR 1,2, or 3

# Ajuste automático con transformación logarítmica (lambda = 0)
mod_auto <- auto.arima(z, seasonal = TRUE, lambda = 0, 
                       stepwise = FALSE, approximation = FALSE, trace = TRUE)

# Mostrar el resumen del modelo
summary(mod_auto)

# Graficar los residuos del modelo
checkresiduals(mod_auto)

#Other tests
mod1=Arima(z,order=c(1,1,1),seasonal=list(order=c(3,1,1),period=12 ),lambda=0)
mod2=Arima(z,order=c(1,1,1),seasonal=list(order=c(5,1,1),period=12 ),lambda=0)
mod3=Arima(z,order=c(1,1,1),seasonal=list(order=c(3,1,3),period=12 ),lambda=0)
mod3_fixed <- Arima(z, order = c(1,1,1), 
                    seasonal = list(order = c(3,1,3), period = 12),
                    lambda = 0,
                    init = c(0.2, 0.1, rep(0, 6))) # AR1, MA1 + 6 seasonal
mod4=Arima(z,order=c(1,1,1),seasonal=list(order=c(5,1,3),period=12 ),lambda=0)
mod4_fixed <- Arima(z, order = c(1, 1, 1), 
                    seasonal = list(order = c(5, 1, 3), period = 12),
                    lambda = 0,
                    optim.control = list(method = "L-BFGS-B", 
                                         lower = c(-0.9, -0.9, -0.9, rep(-1, 8)),
                                         upper = c(0.9, 0.9, 0.9, rep(1, 8))))
mod5=Arima(z,order=c(2,1,1),seasonal=list(order=c(3,1,1),period=12 ),lambda=0)
mod6=Arima(z,order=c(2,1,1),seasonal=list(order=c(5,1,1),period=12 ),lambda=0)
mod7=Arima(z,order=c(2,1,1),seasonal=list(order=c(3,1,3),period=12 ),lambda=0)
mod8=Arima(z,order=c(2,1,1),seasonal=list(order=c(5,1,3),period=12 ),lambda=0)
mod9=Arima(z,order=c(3,1,1),seasonal=list(order=c(3,1,1),period=12 ),lambda=0)
mod10=Arima(z,order=c(3,1,1),seasonal=list(order=c(5,1,1),period=12 ),lambda=0)
mod11=Arima(z,order=c(3,1,1),seasonal=list(order=c(3,1,3),period=12 ),lambda=0)
mod12=Arima(z,order=c(3,1,1),seasonal=list(order=c(5,1,3),period=12 ),lambda=0)

summary(mod7)

for (k in 1:(n.data-n.min))
{
  fit.mod5=Arima(z[1:(n.min+k-1)],order=c(2,1,1),seasonal=list(order=c(3,1,1),period=12 ),lambda=0)   
  forcast.mod5 <- forecast(fit.mod5, h=1)[['mean']]
  fit.mod6=Arima(z[1:(n.min+k-1)],order=c(2,1,1),seasonal=list(order=c(5,1,1),period=12 ),lambda=0)
  forcast.mod6 <- forecast(fit.mod6, h=1)[['mean']]
  fit.mod7=Arima(z[1:(n.min+k-1)],order=c(2,1,1),seasonal=list(order=c(3,1,3),period=12 ),lambda=0)
  forcast.mod7 <- forecast(fit.mod7, h=1)[['mean']]
  fit.mod8=Arima(z[1:(n.min+k-1)],order=c(2,1,1),seasonal=list(order=c(5,1,3),period=12 ),lambda=0)   
  forcast.mod8 <- forecast(fit.mod8, h=1)[['mean']]
  fit.mod9=Arima(z[1:(n.min+k-1)],order=c(3,1,1),seasonal=list(order=c(3,1,1),period=12 ),lambda=0)   
  forcast.mod9 <- forecast(fit.mod9, h=1)[['mean']]
  fit.mod10=Arima(z[1:(n.min+k-1)],order=c(3,1,1),seasonal=list(order=c(5,1,1),period=12 ),lambda=0)
  forcast.mod10 <- forecast(fit.mod10, h=1)[['mean']]
  fit.mod11=Arima(z[1:(n.min+k-1)],order=c(3,1,1),seasonal=list(order=c(3,1,3),period=12 ),lambda=0)
  forcast.mod11 <- forecast(fit.mod11, h=1)[['mean']]
  fit.mod12=Arima(z[1:(n.min+k-1)],order=c(3,1,1),seasonal=list(order=c(5,1,3),period=12 ),lambda=0)   
  forcast.mod12 <- forecast(fit.mod12, h=1)[['mean']]
  MSE.vec5[k]=(z[(n.min+k)]-forcast.mod5)^2
  MAE.vec5[k]=abs(z[(n.min+k)]-forcast.mod5)
  MSE.vec6[k]=(z[(n.min+k)]-forcast.mod6)^2
  MAE.vec6[k]=abs(z[(n.min+k)]-forcast.mod6)
  MSE.vec7[k]=(z[(n.min+k)]-forcast.mod7)^2
  MAE.vec7[k]=abs(z[(n.min+k)]-forcast.mod7)
  MSE.vec8[k]=(z[(n.min+k)]-forcast.mod8)^2
  MAE.vec8[k]=abs(z[(n.min+k)]-forcast.mod8)
  MSE.vec9[k]=(z[(n.min+k)]-forcast.mod9)^2
  MAE.vec9[k]=abs(z[(n.min+k)]-forcast.mod9)
  MSE.vec10[k]=(z[(n.min+k)]-forcast.mod10)^2
  MAE.vec10[k]=abs(z[(n.min+k)]-forcast.mod10)
  MSE.vec11[k]=(z[(n.min+k)]-forcast.mod11)^2
  MAE.vec11[k]=abs(z[(n.min+k)]-forcast.mod11)
  MSE.vec12[k]=(z[(n.min+k)]-forcast.mod12)^2
  MAE.vec12[k]=abs(z[(n.min+k)]-forcast.mod12)
}

Results.mat=rbind(
  apply(cbind(MSE.vec5,MSE.vec6,MSE.vec7,MSE.vec8,MSE.vec9,MSE.vec10,MSE.vec11,MSE.vec12)[-280,],2,mean),
  apply(cbind(MAE.vec5,MAE.vec6,MAE.vec7,MAE.vec8,MAE.vec9,MAE.vec10,MAE.vec11,MAE.vec12)[-280,],2,mean)
)
dimnames(Results.mat)=list(c("MSE","MAE"),c("Model 5", "Model 6","Model 7","Model 8", "Model 9", "Model 10", "Model 11", "Model 12"))

Results.mat

summary(modelobien)
summary(mod5)
summary(mod6)
summary(mod7) #mejor?
summary(mod8)
summary(mod9)
summary(mod10)
summary(mod11)
summary(mod12)

modusurpador=mod7

summary(modusurpador)
summary(modelobien)

Predic.mod=forecast(modusurpador,10)
Predic.mod
plot(Predic.mod,20)

Predic.mod=forecast(modelobien,10)
plot(Predic.mod,20)

length(z)
n.data=length(z)
fit.mod=modusurpador
forcast.mod <- forecast(fit.mod, h=12) #predicciones
plot(seq(390,408), z[390:408],ylab="Registrations",xlab="",type="l",main="Spanish vehicle registration series, January 1960-December 1999",ylim=c(3,4))
points(seq(397,408),forcast.mod$mean,type="l",col="red") # predictions
points(seq(397,408),forcast.mod$lower[,2],type="l",col="blue") # lower CI
points(seq(397,408),forcast.mod$upper[,2],type="l",col="blue") # up CI 

plot(seq(1,50), z[250:299],ylab="registrations",xlab="",type="l",main="Spanish vehicle registration series, January 1960-December 1999")
points(seq(1,50),forecast(modelobien)$fitted[250:299],col="red",type="l")
plot(seq(1,50), z[250:299],ylab="registrations",xlab="",type="l",main="Spanish vehicle registration series, January 1960-December 1999")
points(seq(1,50),forecast(modusurpador)$fitted[250:299],col="blue",type="l")
#plot(seq(1,50), z[200:249],ylab="registrations",xlab="",type="l",main="Spanish vehicle registration series, January 1960-December 1999")
#points(seq(1,50),(forecast(modusurpador)$fitted[200:249]+forecast(modelobien)$fitted[200:249])/2,col="brown",type="l")

summary(modusurpador)
par(mfrow = c(1, 2))

z[0:50]



