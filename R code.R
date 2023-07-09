y=c(9.01,3.06,4.21,1.76,8.00,14.98,1.80,35.26,2.17,9.13,2.92,1.68,1.26,13.58,8.31,34.64,0.07,2.56,2.22,2.00,2.56,32.47,3.85,6.59,0.68,3.14,6.17,3.43,12.19,4.25,4.46,8.93,42.19,4.72,0.65,14.54,0.31,11.57,2.62,3.61,13.69,0.96,5.76,6.10,5.35,1.89,2.09,1.76,3.99,0.18,18.02,19.20,8.98,16.53,33.71,46.50,17.49,22.78,0.77,13.26,15.74,3.67,26.21,1.44,28.59,15.11,2.28,2.24,5.89,7.45,4.64,18.20,3.90,24.74,0.59,2.89,3.25,0.51,10.50,24.85,43.73,9.27,3.65,0.00,13.86,8.24,2.73,4.19,0.66,19.50,3.27,25.02,14.08)
mean(y)

##Specifying_Distribution
install.packages('SMPracticals')
library(SMPracticals)
hist(y,breaks=10,freq=FALSE,main = "Histogram of Y")
curve(dexp(x,(1/9.865)),0,50,add=TRUE)
qqexp(y,line=TRUE)
sd(y)

##Obtaining_MLE
n=100
m=7
mu=sum(y)
s=5
lamda_mle=(n-m)/(mu+(s*m))
lamda_mle

## EM_algorithm_for_finding_maximum_likelihood_estimates ##

em_mle<-function(t0)
{
  theta_initial<-t0
  
  ## update the theta_initial after replacing missing value ##
  
  theta_update<-100/(917.45+35+(7/theta_initial))
  
  ## Calculate the difference ##
  
  diff<-abs(theta_update-theta_initial)
  
  ## set the counter and epsilon ## 
  
  s<-1
  
  epsilon<-.0001
  
  while(diff>.0001)
  {
    theta_initial<-theta_update
    theta_update<-100/(917.45+35+(7/theta_initial))
    diff<-abs(theta_update-theta_initial)
    s<-s+1
  }
  return(data.frame(Iterations=s,Estimate=theta_update))
  
}

## running the code for different initial settings ##

em_mle(.5)
em_mle(.6)
em_mle(.8)
em_mle(1)
em_mle(1.2)
em_mle(1.5)
em_mle(2)
em_mle(20)


##Finding which estimate fit the data well
hist(y,breaks=10,freq=FALSE,main = "Histogram of Y")
curve(dexp(x,0.09764292),0,50,add=TRUE)
hist(y,breaks=10,freq=FALSE,main = "Histogram of Y")
curve(dexp(x,0.09764498),0,50,add=TRUE,col='red')





