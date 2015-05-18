#run with 'Rscript decisionTree.R'
#set.seed(5)

entropy <- function(data)
{
   cnt <- length(unique(data))
   if(cnt == 0)
   {
      return(0)
   }
   t <- ftable(data)
   
   num <- length(data)
   total <- 0

   for(i in 1:cnt)
   {
      if(t[i] != 0)
      {
         total <- total + t[i]/num * log2(t[i]/num)
      }
   }

   return(-total)
}

gains <- function(data, entropy)
{
   total <- 0
   l <- levels(data[,1])

   for(i in 1:length(l))
   {
      d <- data[data[,1] == l[i],]
      total <- total + (length(d[,1]) / length(data[,1])) * entropy(d[,2])
   }

   return(total)
}

#sets up the data and trains it
builder <- function(trainData, recursive=TRUE)
{
#print(length(trainData[,1]))
#print(colnames(trainData))
#   #print(ncol(trainData) <= 2)
#   #print(length(unique(trainData[,ncol(trainData)])) <= 1)
   if(ncol(trainData) < 2 || length(unique(trainData[,ncol(trainData)])) < 1)
   {
      #print("FAILURE!!!")
      #print(trainData)
      return()
   }
   if(ncol(trainData) == 2 || length(unique(trainData[,ncol(trainData)])) == 1)
   {
      features <- unique(trainData[,1])
      classes <- unique(trainData[,2])
      max <- 0;

      attr <- colnames(trainData)[1]
      #print(attr)
      for(i in 1:length(features))
      {
         classes <- unique(trainData[trainData[,1] == features[i],ncol(trainData)])
	 chosenC <- NULL
         for(j in 1:length(classes))
	 {
	    tmp <- length(trainData[trainData[,1] == features[i],classes[j]])
	    #print(tmp)
	    #print(max)
	    if(tmp > max)
	    {
	       max <- tmp
	       chosenC <- classes[j]
	    }
	 }
#	 print("Terminal")
#	 print(chosenC)
	 #print(c(features[i], c(chosenC, NULL)))
	 #if(chosenC != NULL)
	 #{
	    listItems <- list(features[i], list(chosenC, NULL))
      	 #}
      }
#print(c(attr, listItems))      
      return(c(attr, listItems))
   }
 
   e <- entropy(trainData[,ncol(trainData)])   
 
   res <- 100
   for(i in 1:(ncol(trainData) - 1))
   {
      tmp <- gains(cbind.data.frame(trainData[,i], trainData[,ncol(trainData)]), e)
      if(tmp < res)
      {
         res <- tmp
         up <- colnames(trainData)[i]
      }
   }

   l <- levels(data[,up])

   edges <- NULL   
   #print(up)
   #print(l)

   for(i in 1:length(l))
   {
      ex <- names(trainData) %in% c(up)
      edges <- c(edges, c(l[i], Recall(trainData[trainData[,up] == l[i],!ex])))
   }
   
   return(list(up, edges))   
}

#A classifier for determining which iris it is
classifier <- function(tree, test)
{
   
   for(i in ncol(test))
   {
      test$tree[[1]]
      n <- tree[[1]]
      tmp <- tree[[2]]
      for(j in seq_along(tmp))
      {
         tmp2 <- tmp[1]
	 #print(test[,n])
	 #print(tmp)
	 #print(tmp[])

	 if(test[,tree[[1]]] == tmp[[1]])
	 {
	    print("TRUE")
	 }
	 
      }
      
   }
}

#tests the training data against the test data
tester <- function(tree, testData)
{
   #sets the number of correct results to 0)
   results <- 0

   #loops through to find all that match
   for(i in 1:nrow(testData))
   {
      if(classifier(tree,testData[i,1:4]) == testData[i,5])
         results <- results + 1
   }
   return(results);
}

discretize <- function(data)
{
   for(i in 1:ncol(data))
   {
      if(is.numeric(data[,i]))
      {
         data[,i] <- cut(data[,i], breaks=4)
      }
   }
   return(data)
}
nametree2 <- function(X, prefix = "")
  if( is.list(X) )
    for( i in seq_along(X) ) { 
      cat( prefix, names(X)[i], "\n", sep="" )
      nametree(X[[i]], paste0(prefix, "  "))
    }

nametree <- function(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = "")
{  
   if( is.list(X) )
   {
      
      for( i in seq_along(X) ) 
      {
         cat( if(i<length(X)) prefix1 else prefix3, X[[i]], "\n", sep="" )
      	 prefix <- if( i<length(X) ) prefix2 else prefix4
      	 nametree(
            X[[i]], 
            paste0(prefix, "├──"),
       	    paste0(prefix, "│  "),
            paste0(prefix, "└──"),
            paste0(prefix, "   ")
      	 )
      }
   }
}

#Reads in the data from a file
#filename <- readline("File Name?: ")
filename <- "../iris.data"
data <- read.csv(filename, header=F)

#scrambles it up
data <- discretize(data[sample(nrow(data)),])

#makes the training dataset (the first 70%)
trainData<- data[c(1:(nrow(data) * .7)),]

#makes the testing dataset (the last 30%)
testData <- data[c((nrow(data)*.7 + 1):nrow(data)),]
#print(data)

tree <- builder(trainData)
#print(tree)
#print(g[[1]])
nametree(tree)

#prints the percentage
print((tester(tree, testData) / nrow(testData) * 100))