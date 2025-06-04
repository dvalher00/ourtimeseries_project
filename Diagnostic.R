
#############################################################################
# Testing the Estimated Noise Sequence, Brockwell and Davis 1.6, pag. 35-40                          #                                                                                               #
#############################################################################
# 1. Install the package "astsa"
# 2. Install the package "portes"
# 3. Install the package "hacks"

library(forecast)

# Test1. Sample autocorrelation function 

S.ACF=function(x)    # evaluate the % of sample autocorrelation out of the interval [-1.96*sqrt(n),  1.96*sqrt(n)]
{
  l.max=round(length(x)/4,0)
  SampleACF=as.numeric(unlist(Acf(x,l.max))[2:(l.max+1)])
  bound.1=1.96/sqrt(length(x))
  out.bounds=length(seq(1,l.max)[abs(SampleACF)>bound.1])   # number of sample acf in (1:l.max) whose absolute values is greater than 1.96*sqrt(n)
  p.out=round(100*out.bounds/l.max,2)                        # percentage of sample ACF ( in 1:l.max) whose absolute value is greater than 1.96*sqrt(n)
  out.A=matrix(numeric(5),ncol=1)
  out.A[,1]=round(c(length(x), l.max, bound.1,out.bounds,p.out),2)
  dimnames(out.A)=list(c("n","h","bound","# out of bounds","% out of bounds"))
  return(out.A)
}



# Test 2  "Turning Point test", "Difference-sign Test","Rank Test")

# Define the functions that provide the test statistics and the corresponding P-value
difference.sign.test <- function(x)
{
  DNAME <- deparse(substitute(x))
  n<-length(x)
  METHOD <- "Difference-Sign Test (NULL:iid)"
  X<-embed(x,2)
  STATISTIC<-sum(X[,2] > X[,1])
  mu <- (n-1)/2
  sigma2 <- (n+1)/12
  PVAL<-2*(1-pnorm(abs(STATISTIC-mu)/sqrt(sigma2)))
  return(c(abs(STATISTIC-mu)/sqrt(sigma2), 
                 p.value = PVAL))
}

turning.point.test <- function(x)
{
  DNAME <- deparse(substitute(x))
  n<-length(x)
  METHOD <- "Turning Point Test (NULL:iid)"
  X<-embed(x,3)
  STATISTIC<-sum((X[,2] > X[,1] & X[,2] > X[,3])|(X[,2] < X[,1] & X[,2] < X[,3]))
  mu <- 2*(n-2)/3
  sigma2 <- (16*n-29)/90
  PVAL<-2*(1-pnorm(abs(STATISTIC-mu)/sqrt(sigma2)))
  return(c(abs(STATISTIC-mu)/sqrt(sigma2), 
              p.value = PVAL)) 
}


rank.test <- function(x)
     {
         DNAME <- deparse(substitute(x))
         n<-length(x)
         METHOD <- "Rank Test (NULL:iid)"
         STATISTIC<-0
         for(i in 1:(n-1))
             for(j in i:n)
                 if(x[j]>x[i]) STATISTIC<-STATISTIC+1
         mu <- n*(n-1)/4
         sigma2 <- n*(n-1)*(2*n+5)/72
         PVAL<-2*(1-pnorm(abs(STATISTIC-mu)/sqrt(sigma2)))
        return(c(abs(STATISTIC-mu)/sqrt(sigma2), 
                                          p.value = PVAL))
       }


##### The three tests together  ###########

NonParametric.Tests=function(x)
{
  A=rbind(turning.point.test(x),
          difference.sign.test(x),
          rank.test(x)
          )
dimnames(A)=list(c("Turning Point test", "Difference-sign Test","Rank Test"),c("Test Statistic", "P-value"))
  return(A)
}  



# Test3. (Ljung-Box test)

# np= number of estimated parameters in the model
My.Ljung.Box=function(x,np)
{
  n=length(x)
  l.max=round(n/4,0)
  SampleACF=as.numeric(unlist(Acf(x,l.max))[2:l.max])
  lag.vec=seq(5,30,length=6)
  Q.ML=numeric(6)
  for (k in 1:6)
  {
    Q.ML[k]=n*(n+2)*sum(  ( (SampleACF[1:lag.vec[k]])^2)/(n-seq(1:lag.vec[k])) )  
  }
  Q.ML.Pvalues=numeric(6)
  for (k in 1:6)
  {
    Q.ML.Pvalues[k]=1-pchisq(Q.ML[k], lag.vec[k]-np, ncp = 0, lower.tail = TRUE, log.p = FALSE)
  }
  Q.ML.out=cbind(lag.vec, Q.ML,Q.ML.Pvalues)
  dimnames(Q.ML.out)=list(NULL,c("k","Test Statistic","P-value"))
  return(Q.ML.out)
}  
  

 #############################################################################
 #   Testing the hipothesis that residuals follows a normal distribution                                                                                                  #
#############################################################################

TEST.normality=function(x)
{
library(nortest)
test.temp=matrix(
round(c(
c(shapiro.test(x)[[1]],lillie.test(x)[[1]], pearson.test(x)[[1]]),
c(shapiro.test(x)[[2]],lillie.test(x)[[2]], pearson.test(x)[[2]])
),4),ncol=2,
)
dimnames(test.temp)=list(c("Shapiro-Wilxs","Lilliefors","Pearson Chi.square"), c
("Statistics", "P-value"))
return(test.temp)
}


Check.normality=function(x)
{
par(mfrow=c(2,2))
qqnorm(x)
qqline(x)
hist(x,xlab="residuals",ylab="",main="",probability=T)
range.d=c(min(x),max(x))
param.d=c(mean(x),sqrt(var(x)))
x.d=seq(range.d[1],range.d[2],length=1000)
dnorm.d=function(x){return(dnorm(x,mean=param.d[1],param.d[2]))}
points(x.d,dnorm.d(x.d),type="l")
return(TEST.normality(x))
}

 
  
  
  

