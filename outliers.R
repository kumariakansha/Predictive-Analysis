library("ggplot2")  #installing ggplot and loading diamond dataset
library(plyr)
diamonds                 #diamond dataset

# question 1.2
str(diamonds)    #description of diamond dtaset
#question 1.3
bestcolor <- data.frame(diamonds[,,])  #coverting diamond dataset into datframe of bestcolor
bestcolor = bestcolor[bestcolor$color=='D',] #selecting rows having color D in the dataframe
bestcolor = bestcolor[order(-bestcolor$carat,-bestcolor$price),]  #ordering first by carat then by price in descending order
print(bestcolor)  # printing the dataframe before removing the color column
 bestcolor= bestcolor[,-3]   #removing the color column
 print(bestcolor)  #printing the data frame after removing the color column
 write.table(bestcolor,"C:/Users/akank/Desktop/akansha_kumari.txt",sep="\t" )  #writing it to a text file
 
 #question 1.4
print(diamonds)
 countESI2  <- function(n) #creating a function 
 { 
   count1 <- 0    #declaring the counter as 0

   for (i in 1:n) #for loop till n ,value of n is passed to function
     {
     
     if((diamonds[i,3]=='E') && (diamonds[i,4]=='SI2'))  #if the condition of color as'E' and clarity as 'SI2' is met
      count1 = count1 +1   #counter value is incremented
      
       
    }
return(count1)  #return the total count of the diamonds where criteria was met
 }
 
countESI2(50) #passing the value of n as 50 to the function
countESI2(150) #passing the value of n as 150 to the function

#question 1.5
data1 <-data.frame(diamonds)
n=50
sum(data1[1:n,]$color=='E' & data1[1:n,]$clarity=='SI2')  #the sum function  is summing up the values where the condition is met

n=150
sum(data1[1:n,]$color=='E' & data1[1:n,]$clarity=='SI2')

#Question2.1
  
data1 <- data.frame(diamonds)
  
expensive = data1[which.max(data1$price),] #storing the row of  maximim value of price in expensive data frame

  cat( "The depth of the diamond:", expensive$z) #printing the depth of the diamond having maximum price
  
  cat("The length of the diamond:", expensive$x) #printing the length of the diamond having maximum price
  cat("the width of the diamond:",expensive$y)   #printing the width of the diamond having maximum price
  size= (expensive$x * expensive$y * expensive$z) #Calculating the size of the diamond having maximum price
  cat("the size of the diamond is:", size) #printing the size of the diamond having maximum price.
  
  #the dimensions of the diamond where the price is 1344
  expensive = data1[which(data1$price==1344),]
  
  cat( "The depth of the diamond:", expensive$z)  #the  depth  dimension of the diamond where the price is 1344
  
  cat("The length of the diamond:", expensive$x) #the  length dimension of the diamond where the price is 1344
  cat("the width of the diamond:",expensive$y)   #the width dimension of the diamond where the price is 1344
  size= (expensive$x * expensive$y * expensive$z)   # calculating the size of the diamond
  cat("the size of the diamond is:", size)          #printing the size of the diamond

  #question 2.2
  
  ideal_cut <-data.frame(diamonds)
  ideal_cut = ideal_cut[which(ideal_cut$cut=='Ideal'),] #selecting the rows where cut is ideal
  ideal_cut = ideal_cut[order(-ideal_cut$price),]   #arranging the rows in descending order
  head(ideal_cut$price,n=7) #selecting the 7 most expensive diamonds havinf ideal cut
  
 # question 2.3
  ideal_cut <-data.frame(diamonds)
  ideal_cut = ideal_cut[which(ideal_cut$cut=='Ideal'),]  #selecting the rows where cut is ideal
  print(ideal_cut)
  temp1=ideal_cut[which.max(ideal_cut$price),]   #checking the max price of the ideal cut 
  print(temp1)
  sum( ideal_cut[1:nrow(ideal_cut),]$clarity== temp1$clarity & ideal_cut[1:nrow(ideal_cut),]$color=='D')
   #calculating no of diamonds having ideal cut ,IF as clarity and D ass color

#question 2.4
  data1 <-data.frame(diamonds)  #forming data frame for the table
  attach(data1)   #attaching the data frame to the table
  mytable <- table(data1$clarity,data1$color) # clarity will be rows, color  will be columns 
  mytable # print table
  
 #question 2.5
  data_temp =diamonds[which(diamonds[,3] =='F' & diamonds[,4]=='VS1' ),]  #selecting the values having F as color and VS1 as clarity
  vs1_elements =nrow(data_temp)    #calculating the total count of the Color F for VS1 clarity
  total_count = nrow(diamonds)     #total no of values is diamond dataset
  probability = (vs1_elements/total_count)  #calculating the probability 
  cat("The probability of VS1 with F color is :", round(probability,digits=4))  #printing the probability ,rounding it of till 4 digits
  
 # question 3
  data1 <-data.frame(diamonds)
  table1<- table(data1$carat,data1$price)
  plot(data1$carat,data1$price)  #plotting X and y graph using carat vs price 
  boxplot(data1$price,main="boxplot of prices of diamond")  #finding the outlier in prices using boxplot
  data1 <-data.frame(diamonds)
  table1<- table(data1$clarity,data1$price)
  barplot(table1,main="Number Of Diamonds with clarity  and Price",xlab="Clarity",ylab="Prices",legend = rownames(table1),beside=TRUE)
  #bargraph clarity vs price.