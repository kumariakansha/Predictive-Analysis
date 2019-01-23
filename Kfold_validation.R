#question1

data1 = read.csv("C:/Users/akank/Downloads/admission.csv")# Load the data of my_admission to my_admission variable
data1 = data1[sample(nrow(data1)),]   #Data partition of the data

kfold=function(x,y,k)
{
  nrFolds <- 10
  
  # generate array containing fold-number for each sample (row)
  folds <- rep_len(1:nrFolds, nrow(data1))
  
  # actual cross validation
  for(k in 1:nrFolds) {
    # actual split of the data
    fold <- which(folds == k)
    trainset <- data1[-fold,]
    testset <- data1[fold,]
    
    
    fit=lm(y~x) #creating single regression model of y and x
    fitmodel<- glm(fit, family = binomial, data = trainset) #using glm function for getting generalized linearmodel of fit model on training data set
    
    prediction1 = predict.glm(fitmodel,testset,type="response",se.fit=TRUE) #testing our model on test data set
    
    prediction1.model = round(prediction1$fit,0) 
  }
  confusionMatrix= table(prediction1.model,fitmodel$y) #creating confusion matrix for the predicted values vs actual values
  missclass  = 1 -sum(diag(confusionMatrix))/sum(confusionMatrix)   #calculating the missclassification rate using the formula
  missclass #Returns the function with misclassification rate
}

kfold(data1$gre,data1$admit,2)

#question2

determinant=function(X,k) 
  {
  if (dim(X)[1] == 1 && dim(X)[2] == 1)
    return(X[1,1])
  if (dim(X)[1] == 2 && dim(X)[2] == 2)
    return(X[1,1]*X[2,2]-X[1,2]*X[2,1]) 
  else
    s = 0
  for (i in 1:dim(X)[2]) {
    sum1 = sum1 + X[k,i]*(-1)^(k+i)*
      determinant(X[-k,-i],k)   #calling the determinant function recursively
  }
  return(s) #returning the sum value to the function
}
M <- matrix(rnorm(3^2), 3, 3) #passing the value of matrix
determinantRek(M,3) # calling the determinate function with passing the value with square matrix order 3
 
#Comparing the value with inbuilt function
det(M)

#Question3 

myfile= read.csv("C:/Users/akank/Downloads/Hardness.csv",header=FALSE) 
Calculate_function1 = function(mean1,std,myfile)
  {
  means = aggregate(hardness,list(hardness$V1),mean)[,3] #Mean CAlculation for each sample
  dataframe1 = data.frame(means) # Data frame created for all the mean value generated for the samples
  dataframe1$sample = c(seq(1,25)) # Inserting the sample that is present in the file
  
  limit_up = mean1+ (3*std/2) #  the  upper limit action  is found with the help of mean and standard deviation.
  limit_down = mean1 - (3*std/2) #   the  lower limit action was found  with the help of mean and standard deviation.
  warning_up = mean1 + (2*std/2) # the warning upper limit was found  with the help of mean and standard deviation.
  warning_down = mean1 - (2*std/2) #  the warning lower limit was found with the help of mean and standard deviation.
  dataframe1f$aboveup = ifelse(dataframe1$means > limit_up,1,0)  # aboveup let us know  that if the mean is higher than  upper limit.
  dataframe1$abovedown = ifelse(dataframe1$means < limit_down,1,0) #  aldown  let us know if the mean is higher than the lower limit.
  dataframe1$warnup = ifelse(dataframe1$means > warning_up,1,0) # warnup  let us know if the mean is higher than warning upper limit.
  dataframe1$warndown = ifelse(dataframe1$means < warning_down,1,0) # warndown let us know if the mean is higher than warning lower limit.
  frame2 = a(dataframe1,limit_up=limit_up,limit_down=limit_down,warning_up=warning_up,warning_down=warning_down)# Create another  dataframe containing the data frame values and the limit values
  class(frame2) = 'shewhart' # Assingning class attribute to the frame2 as shewhart
  return(frame2) # return the object frame2  which has the list of "Sample,Means  and also whether the  mean is higher or lower to limits"

  plot.shewhart = function(df.o){ # create the plot function inside shewhart class 
    x = df$sample # it takes sample as input
    y = df$means # it tales means as y axis input
    }
   
    plot(x, y, main="Shewhart Control Chart ", xlab="Sample ", ylab="Mean ", pch=19) #plotting by the scatter plot
    abline( h = c(1,mean(df$means)), col=c("green"), lty = 3)
    plot(x, y, type = "p", pch = 4, col = "red", ylim = c(120,140),main = "Shewhart Control Chart",xlab = "Samples", ylab = "Mean")
    lines(x, y, lty = 3, col = "black" )
  }
  
Calculate_function1(127,3.4,myfile)
