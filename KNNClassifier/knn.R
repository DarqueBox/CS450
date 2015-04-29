#run with 'Rscript shell.R'

#sets up the data and trains it
trainer <- function(trainData)
{
   return(trainData);
}

#A classifier for determining which iris it is
classifier <- function(data, test, n = 8)
{
   distances <- NULL

   #finds all the distances
   for(i in 1:nrow(data))
   {
      distances[i] <- dist(rbind(data[i,1:4], test), method="euclidean")
   }

   #attaches the iris type onto the distances
   results <- data.frame(distances, data[,5])

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
      if(classifier(trainData,testData[i,1:4]) == testData[i,5])
         results <- results + 1
   }
   return(results);
}

#Reads in the data from the iris file
data <- read.csv("../iris.data", header=F)

#scrambles it up
data <- data[sample(nrow(data)),]

#makes the training dataset (the first 70%)
trainData <- data[c(1:(nrow(data) * .7)),]

#makes the testing dataset (the last 30%)
testData <- data[c((nrow(data)*.7 + 1):nrow(data)),]

#prints the percentage
print((tester(trainer(trainData), testData) / nrow(testData) * 100))