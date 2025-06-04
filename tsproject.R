library(forecast)
source("BoxCoxTransformation.R")

data=read.table("data9.txt",header=T)

plot.ts(data[,1],xlab="Months", ylab="Dollars",main="Total monthly expenditure on cafes, restaurants and takeaway food services in Australia, April 1980 - April 2015")
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

par(mfrow=c(1,1))
ts.plot(X.tilde ,ylab="",xlab="",type="l",main="Logarithm of the registration series")

Acf(X.tilde,main="ACF of the logarithm of the registration series",xlab="Lag",ylab="ACF")

Wt.1=diff(X.tilde,lag=1,differences=1) 

ts.plot(Wt.1,xlab="t", ylab="",main="First difference of the logarithm of the registration series",type="l") 

ggseasonplot(Wt.1, main=" ")

Acf(Wt.1,40, main="ACF of the first difference of the logarithm of the registration series",xlab="Lag",ylab="ACF")

ts.plot(Wt.1[1:48],xlab="t", ylab="",main="",type="b")  
points(seq(13,24),Wt.1[13:24],type="b",col="blue",lwd=1.4)
points(seq(25,36),Wt.1[25:36],type="b",col="red",lwd=1.4)
points(seq(37,48),Wt.1[37:48],type="b",col="green",lwd=1.4)

Wt.2=diff(Wt.1, lag=12,differences=1) 

ts.plot(Wt.2,xlab="t", ylab="",main="Logarithm of the registration series after one  regular difference and one seasonal difference",type="l") 

Acf(Wt.2, 40, main="",xlab="Lag",ylab="ACF")

Pacf(Wt.2, 40, main="",xlab="Lag",ylab="PACF")

mod1=Arima(data,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12 ),lambda=0)
mod2=Arima(data,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12 ),lambda=0)
mod3=Arima(data,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12 ),lambda=0)

AIC.BIC=rbind(c(mod1$"aic",mod1$"aicc",mod1$"bic"),
              c(mod2$"aic",mod2$"aicc",mod2$"bic"),
              c(mod3$"aic",mod3$"aicc",mod3$"bic"))
dimnames(AIC.BIC)=list(c("Model 1","Model 2", "Model 3"),c("AIC","AICc","BIC"))
AIC.BIC

n.min=200  # minimal sample size  required to estimate a model
n.data=length(z)
MSE.vec1=numeric(n.data-n.min) # Mean Squared Error model 1
MAE.vec1=numeric(n.data-n.min) # Mean Absolute Error model 1
MSE.vec2=numeric(n.data-n.min) # Mean Squared Error model 2
MAE.vec2=numeric(n.data-n.min) # Mean Absolute Error model 2
MSE.vec3=numeric(n.data-n.min) # Mean Squared Error model 3
MAE.vec3=numeric(n.data-n.min) # Mean Absolute Error model 3


for (k in 1:(n.data-n.min))
{
  fit.mod1=Arima(z[1:(n.min+k-1)],order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12 ),lambda=0)   
  forcast.mod1 <- forecast(fit.mod1, h=1)[['mean']]
  #
  fit.mod2=Arima(z[1:(n.min+k-1)],order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12 ),lambda=0)
  forcast.mod2 <- forecast(fit.mod2, h=1)[['mean']]
  fit.mod3=Arima(z[1:(n.min+k-1)],order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12 ),lambda=0)
  forcast.mod3 <- forecast(fit.mod3, h=1)[['mean']]
  MSE.vec1[k]=(z[(n.min+k)]-forcast.mod1)^2
  MAE.vec1[k]=abs(z[(n.min+k)]-forcast.mod1)
  MSE.vec2[k]=(z[(n.min+k)]-forcast.mod2)^2
  MAE.vec2[k]=abs(z[(n.min+k)]-forcast.mod2)
  MSE.vec3[k]=(z[(n.min+k)]-forcast.mod3)^2
  MAE.vec3[k]=abs(z[(n.min+k)]-forcast.mod3)
}

# Summary Results 
Results.mat=rbind(
  apply(cbind(MSE.vec1,MSE.vec2,MSE.vec3)[-280,],2,mean),
  apply(cbind(MAE.vec1,MAE.vec2,MAE.vec3)[-280,],2,mean)
)
dimnames(Results.mat)=list(c("MSE","MAE"),c("Model 1","Model 2", "Model 3"))

Results.mat

require(astsa)         # load astsa
source("Diagnostic.R")  #(load the functions in "diagnostic")
require(portes)       # load portes  (Ljung-Box test, McLeod-Li_83 test)
# require(Nortest)

S.ACF(mod2$residuals)

NonParametric.Tests(mod2$residuals)  

My.Ljung.Box(mod2$residuals,4)

Check.normality(mod2$residuals)

S.ACF(mod1$residuals)

NonParametric.Tests(mod1$residuals)   

My.Ljung.Box(mod1$residuals,3)

Check.normality(mod1$residuals)

n.res=length(mod1$residuals)
t.obs=mean(mod1$residuals)/ (var(mod1$residuals)/(n.res-1))  
p.value=1-pnorm(t.obs)
p.value

plot(seq(1,408),data[,1],ylab="registrations",xlab="",type="l",main="Spanish vehicle registration series, January 1960-December 1999")
points(seq(1,408),forecast(mod1)$fitted,col="red",type="l")

n.data=length(z)
fit.mod1=Arima(z[1:468],order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12 ),lambda=0)   
forcast.mod1 <- forecast(fit.mod1, h=12) #predicciones

Predic.mod1=forecast(mod1,10)
Predic.mod1

plot(Predic.mod1,20)
