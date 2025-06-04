###  Functions to implememt Box-Cox transformation (for details see Lab 5)


####  Plot.var(x,n.tilde)  
# x: observed time series
# n.tilde: number of observations per group
Plot.var=function(x,n.tilde)  
{
  A.temp=matrix(x,ncol=n.tilde,byrow=T)     # Each row correspond to a group of n.tilde consecutive observations 
  xbar.vec=apply(A.temp,1,mean)       # vector of means 
  s.vec=sqrt(apply(A.temp,1,var))     # vector of standard deviations 
  plot(log(xbar.vec),log(s.vec),xlab="log(mean)",ylab="log(standard deviation)",main=paste("Number of observations per group:",n.tilde),type="p")
}



####  BoxCox(x,n.tilde)  
# x: observed time series
# n.tilde: number of observations per group
BoxCox=function(x,n.tilde)
{
  A.temp=matrix(x,ncol=n.tilde,byrow=T)    # Each row correspond to a group of n.tilde consecutive observations 
  xbar.vec=apply(A.temp,1,mean)      # vector of means 
  s.vec=sqrt(apply(A.temp,1,var))    # vector of standard deviations 
  lambda=1-lm(log(s.vec)~log(xbar.vec))$coefficients[2]
  yt=(x^lambda-1)/lambda
  lambda1=round(lambda,2)
  ts.plot(yt,xlab="",ylab="",main=paste("Transformed series obtained using the Box-Cox transformation with lambda=",lambda1),type="l")
}





####  BoxCox.out(x,n.tilde)  
# x: observed time series
# n.tilde: number of observations per group

BoxCox.out=function(x,n.tilde)
{
  A.temp=matrix(x,ncol=n.tilde,byrow=T)   
  xbar.vec=apply(A.temp,1,mean)       
  s.vec=sqrt(apply(A.temp,1,var))
  lambda=1-lm(log(s.vec)~log(xbar.vec))$coefficients[2]
  yt=(x^lambda-1)/lambda
  return(list(yt,lambda))
}

