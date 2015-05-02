#run with 'Rscript knnCar.R'

#sets up the data and trains it
trainer <- function(trainData)
{
   return(trainData);
}

#A classifier for determining which iris it is
classifier <- function(data, test, n = 9)
{
   d<- NULL
   #finds all the distances

   for(i in 1:nrow(data))
   {
      d[i] <- dist(rbind(data[i,1:ncol(data)-1], test), method="euclidean")
   }

   #attaches the iris type onto the distances
   results <- data.frame(d, data[,ncol(data)])

   #sorts
   indexes <- order(results[,1], decreasing=F)[1:n]
   
   #returns the one with the highest frequency
   return(names(sort(table(results[c(indexes),2]), decreasing=T))[1])
}

#tests the training data against the test data
tester <- function(data, testData)
{
   #sets the number of correct results to 0)
   results <- 0
   
   #loops through to find all that match
   for(i in 1:nrow(testData))
   {
      if(classifier(data,testData[i,1:ncol(data)-1]) == testData[i,ncol(testData)])
         results <- results + 1
   }

   #prints the percentage
   print('My KNN implementation')
   print(results / nrow(testData) * 100)
}

normalize <- function(data)
{
   data <- cbind(scale(data[,1:ncol(data)-1]), as.numeric(data[,ncol(data)]))
   
   return(data)
}

prepCarData <- function(data)
{
   data[,1] <- as.numeric(
                  ordered(data[,1],c("vhigh", "high", "med", "low"),c(4, 3, 2, 1))
               )
   data[,2] <- as.numeric(
                  ordered(data[,2],c("vhigh", "high", "med", "low"),c(4, 3, 2, 1))
               )
   data[,3] <- as.numeric(
                  ordered(data[,3],c("5more",4,3,2,1),c(5,4,3,2,1))
               )
   data[,4] <- as.numeric(
                  ordered(data[,4],c("more",4,3,2,1),c(5,4,3,2,1))
               )
   data[,5] <- as.numeric(
                  ordered(data[,5],c("big","med","small"),c(3,2,1))
               )
   data[,6] <- as.numeric(
                  ordered(data[,6],c("vhigh", "high", "med", "low"),c(4, 3, 2, 1))
               )

   return(data)
}

#Reads in the data from the car file
data <- read.csv("../car.data", header=F)

data <- prepCarData(data)

#scrambles it up
data <- data[sample(nrow(data)),]

#makes the training dataset (the first 70%)
trainData <- normalize(data[c(1:(nrow(data) * .7)),])

#makes the testing dataset (the last 30%)
testData <- normalize(data[c((nrow(data)*.7 + 1):nrow(data)),])

#tests
tester(trainData, testData)

#knn lib
library(FNN)
cl <- factor(trainData[,ncol(trainData)])

a <- knn(data.frame(trainData[,1:ncol(trainData)-1]), data.frame(testData[,1:ncol(testData)-1]), cl, k=9)

cnt <- 0
for(i in 1:nrow(testData))
{
   if(a[i] == testData[i,ncol(testData)])
      cnt <- cnt + 1
}
print('FNN Library KNN implementation')
print(cnt / nrow(testData) * 100)
