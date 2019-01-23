#17200831-akansha kumari
#question 2

rm(list=ls())

#Answer 2(a)
wines <- read.csv("wines.csv",header=T,as.is=T)
plot(wines)
#when we pass the whole data frame in the plot function, it calls the pair function
#and ceates pairs of x an y for the for the scatter plot, resulting in a matrix of a scatterplot.

#ANSWER 1(B)
x<-wines$Tasting
y<-wines$View

mod<-(x~y)
mod1 <- lm(x~y + I(y^2) )
#quadratic equation is producing better intercept values
#Answr 1(c)
par(mfrow=c(1,2))
plot(y,mod$fitted.values,type="n",main = "Linear Regression")
lines(y,mod$fitted.values,col='red')
plot(y,mod1$fitted.values,type="n",main = "Quadratic Regression")
lines(y,mod1$fitted.values,col='red')

#Answer 2 PART (B.a)


rosenbrock<-function(x)
{
  stopifnot(is.vector(x,mode = "numeric")) #there is check that value should be numeric
  n<-length(x)  #calculating the length of the vector x
  res <-0  #defining res as 0
  for(i in 1:(n-1))  #starting the loop for calculation for optimization iteration
  {
  res<-res+100*(x[i+1]-x[i]^2)^2+(1-x[i])^2  #the  formula to calculate the gradient's optimized value /the rosebrock formula for optimization
  }
  return(res)  #returning the optimized values
}
x<-rnorm(4,3,2)  #passing the rnorm values to x
 print(rosenbrock(x))  #calling the rosenbrock function to find its gradient's optimized value.
 
 #
 
