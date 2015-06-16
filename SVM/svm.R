library(e1071)


#Reads in the data from a file
#filename <- readline("File Name?: ")
filename <- "../abalone.csv"
data <- read.csv(filename, header=T)

#scrambles it up
data <- data[sample(nrow(data)),]

#makes the training dataset (the first 70%)
trainData<- data[c(1:(nrow(data) * .7)),]

#makes the testing dataset (the last 30%)
testData <- data[c((nrow(data)*.7 + 1):nrow(data)),]


s <- svm(Rings~., data = trainData, gamma = .001, cost = .0001)

prediction <- predict(s, testData[,-9])

tab <- table(pred = prediction, true = testData[,9])
dia <- sum(diag(tab))
sum <- sum(tab)

print(dia/sum)