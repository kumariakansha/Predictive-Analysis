#17200831-akansha kumari question 1

rm(list=ls())

#QUESTION 1(a)
crime <- read.csv("EurostatCrime2015.csv",row.names=1,header=T,as.is=T)

 max(crime["Ireland",])
 min(crime["Ireland",])
 
 #The maximum crime is of Theft which is 1500 and minimum is of homicide which is 1.32
 
 #Question 1(b)
 
 cor(crime[,1:7],use="complete.obs")
 #initially the correlation function didn't work because we had NA values.
 #So, when we use complete.obs, we discard the entire row if an NA.
 
 #Question 1(c)
 crime$sexual <- crime$Rape +crime$Sexual.assault + crime$Sexual.violence
 #creating a new column sexual for all sum of all the sexual related activities
 crime <- crime[-c(3,5,6)]
 #removing the columns having the value rape,sexual.assault,sexual violence
 
 #Question 1(d)
 which(!rowSums(!is.na(crime))) 
 #prints the country names which have NA values in their rows
 crime <-crime[!(rowSums(is.na(crime))),]
 #deletes the countries that have NA values present in it.
 
 #Question 1(e)
 crime1 <- read.csv('EurostatCrime2008.csv',row.names=1,header=T,as.is=T)
 crime2 <- read.csv('EurostatCrime2009.csv',row.names=1,header=T,as.is=T)
 
 crime3 <- read.csv('EurostatCrime2010.csv',row.names=1,header=T,as.is=T)
 crime4 <- read.csv('EurostatCrime2011.csv',row.names=1,header=T,as.is=T)
 crime5 <- read.csv('EurostatCrime2012.csv',row.names=1,header=T,as.is=T)
 crime6 <- read.csv('EurostatCrime2013.csv',row.names=1,header=T,as.is=T)
 crime7 <- read.csv('EurostatCrime2014.csv',row.names=1,header=T,as.is=T)
crime8<- merge( crime,c(crime1,crime2,crime3,crime4,crime5,crime6,crime7) ,by="row.names",all=T)
 #merged all files by rownames i.e countries
 
 #question 1(f)