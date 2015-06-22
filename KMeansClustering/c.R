library(datasets)
library(cluster)

data <- state.x77

#HC
distance <- dist(as.matrix(data))
hc <- hclust(distance)
jpeg("hc.jpg")
plot(hc)

#HC Normalized
dataNorm <- scale(data)
distNorm <- dist(as.matrix(dataNorm))
hcNorm <- hclust(distNorm)
jpeg("hcNorm.jpg")
plot(hcNorm)

#HC Without Area
dataNormWOArea <- subset(dataNorm, select = -Area)
distNormWOArea <- dist(as.matrix(dataNormWOArea))
hcNormWOArea <- hclust(distNormWOArea)
jpeg("hcNormWOArea.jpg")
plot(hcNormWOArea)

#HC Only Frost
dataNormOFrost <- subset(dataNorm, select = Frost)
distNormOFrost <- dist(as.matrix(dataNormOFrost))
hcNormOFrost <- hclust(distNormOFrost)
jpeg("hcNormOFrost.jpg")
plot(hcNormOFrost)


#C 3 clusters
c <- kmeans(dataNorm, 3)
jpeg("c3.jpg")
clusplot(dataNorm, c$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#C 1-25
cArray <- NULL

for(i in 1:25)
{   
    cArray[i] = kmeans(dataNorm, i)$tot.withinss   
}

#Elbow method
jpeg("elbow.jpg")
plot(cArray)

#List
elC <- kmeans(dataNorm, 5)

#Cusplot
jpeg("c5.jpg")
clusplot(dataNorm, elC$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
