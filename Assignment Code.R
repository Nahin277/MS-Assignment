x=c(7, 3, 4, 11, 2, 7, 0, 2, 8, 1, 2, 4, 7, 6, 4, 7, 4, 3, 6, 3, 2, 6, 6, 1, 3, 7, 2,3, 1, 3, 8, 7, 5, 3, 5, 9, 11, 7, 8, 6, 7, 6, 7, 2, 4, 7, 5, 4, 6, 7)

y=c(10.06, 12.76, 4.94, 8.52, 8.17, 13.82, 6.31, 8.27, 15.45, 9.54, 12.68, 6.84, 8.23, 11.28, 10.19, 13.69, 11.70, 10.54, 10.70, 7.17, 10.60, 13.60, 14.11, 14.82, 15.94, 6.49, 9.63, 7.77,11.38, 4.78, 13.31, 11.10, 8.10, 9.81, 11.64, 14.02, 4.58, 12.17, 7.25, 10.52, 11.64, 8.06,10.42, 9.21, 9.43, 11.17, 10.31, 10.22, 12.42, 9.27)

fit=lm(y~x)
summary(fit)
plot(x,y)
res=residuals(fit)
sd(res)

library(invgamma)

sim<-function(a,b,c,d,e,f,x,y,alpha0,beta0,sig0)
{
  n=length(x)
  
  X=matrix(rep(0,3000),1000,3)
  colnames(X)=c("alpha","beta","sigma")
  
  X[1,]=c(alpha0,beta0,sig0)
  
  for(i in 2:1000)
  {
    X[i,1]=rnorm(1,(b*sum(y)-b*X[i-1,2]*sum(x)+a*X[i-1,3])/(X[i-1,3]+n*b),sqrt((b*X[i-1,3])/(X[i-1,3]+n*b)))
    X[i,2]=rnorm(1,(d*sum(x*y)-X[i,1]*d*sum(x)+X[i-1,3]*c)/(d*sum(x^2)+X[i-1,3]),sqrt((X[i-1,3]*d)/(d*sum(x^2)+X[i-1,3])))
    X[i,3]=rinvgamma(1,(n/2)+e,(sum((y-X[i,1]-X[i,2]*x)^2)+2*f)/2)
    
  }
  return(X)
}


x=c(7, 3, 4, 11, 2, 7, 0, 2, 8, 1, 2, 4, 7, 6, 4, 7, 4, 3, 6, 3, 2, 6, 6, 1, 3, 7, 2,3, 1, 3, 8, 7, 5, 3, 5, 9, 11, 7, 8, 6, 7, 6, 7, 2, 4, 7, 5, 4, 6, 7)
y=c(10.06, 12.76, 4.94, 8.52, 8.17, 13.82, 6.31, 8.27, 15.45, 9.54, 12.68, 6.84, 8.23, 11.28, 10.19, 13.69, 11.70, 10.54, 10.70, 7.17, 10.60, 13.60, 14.11, 14.82, 15.94, 6.49, 9.63, 7.77,11.38, 4.78, 13.31, 11.10, 8.10, 9.81, 11.64, 14.02, 4.58, 12.17, 7.25, 10.52, 11.64, 8.06,10.42, 9.21, 9.43, 11.17, 10.31, 10.22, 12.42, 9.27)

z=sim(9.8,0.72,0.08,0.02,2,7.5,x,y,9,0.05,7)
z

#end
m=cbind(mean(z[,1]),mean(z[,2]),mean(z[,3]))
m



## Plot the marginal distribution of alpha,beta and sigma ##

par(mfrow=c(2,1))
ts.plot(z[,1])
abline(h=10)
acf(z[,1],lag=50)

par(mfrow=c(2,1))
ts.plot(z[,2])
abline(h=64)
acf(z[,2],lag=50)

par(mfrow=c(2,1))
ts.plot(z[,3])
abline(h=10)
acf(z[,3],lag=50)

## To get independent sample##
## This is called thinning ##

s<-seq(1,1000,4)
z1<-z[,1][1:1000]
z1<-z1[s]

par(mfrow=c(2,1))
ts.plot(z1)
abline(h=10)
acf(z1,lag=50)

s2<-seq(1,1000,6)
z2<-z[,2][1:1000]
z2<-z2[s2]

par(mfrow=c(2,1))
ts.plot(z2)
abline(h=64)
acf(z2,lag=50)


z3<-z[,3][1:1000]
z3<-z3[s]

par(mfrow=c(2,1))

ts.plot(z3)
abline(h=64)
acf(z3,lag=50)