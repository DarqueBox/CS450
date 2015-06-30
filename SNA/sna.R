library(igraph)
dat=read.csv("Mommy_twitterMentions.csv",header=TRUE) # choose an edgelist in .csv file format
el=as.matrix(dat) # coerces the data into a two-column matrix format that igraph likes
el[,1]=as.character(el[,1])
el[,2]=as.character(el[,2])
g=graph.edgelist(el,directed=FALSE) # turns the edgelist into a 'graph object'
#jpeg("mommyBlog.jpg")
#plot(g)

d <- degree(g)
print("Degree")
print(sort(d,decreasing=T)[1])

c <- closeness(g)
print("Closeness")
print(sort(c,decreasing=T)[1])

b <- betweenness(g)
print("Betweeness")
print(sort(b,decreasing=T)[1])