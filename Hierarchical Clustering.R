# Topic 3
library(caret)
library(mlbench)
data("Zoo")


data = Zoo[,-17] # 17th column is a class label 

#Compute the dissimilarity distance matrix of the dataset using three different methods:
#Euclidean, Manhattan and Jaccard.
#Provide in the following space the distances between 
#chicken and flamingo and between 
#chicken and squirrel
library(vegan)
matrix1 = vegdist(data,method = "euclidean")

distMatrix <- as.matrix(matrix1)
distMatrix["chicken","flamingo"] # 1.414214
distMatrix["chicken","squirrel"] # 2.645751

matrix2 = vegdist(data,method = "manhattan")
distMatrix2 <- as.matrix(matrix2)
distMatrix2["chicken","flamingo"] #2
distMatrix2["chicken","squirrel"] #7 

matrix3 = vegdist(data,method = "jaccard")
distMatrix3 <- as.matrix(matrix3)
distMatrix3["chicken","flamingo"] #0.2
distMatrix3["chicken","squirrel"] # 0.5833333


#Perform agglomerative hierarchical clustering using the Jaccard dissimilarity distance matrix
#and both complete and single linkage

single = hclust(matrix3,method = "single")
plot(as.dendrogram(single))
singleplot = as.dendrogram(single)


complete = hclust(matrix3,method = "complete")
plot(as.dendrogram(complete))
completeplot = as.dendrogram(complete) 


# We use the $height to check the dendogram heights and sort them
# The biggest value is the height of the first split. Second biggest is second split, etc.

sort(single$height) #7th biggest height 0.3
sort(complete$height) #7th biggest height 0.6666667



plot(singleplot)+
  abline(h = 0.3, lty = 2, col = "red" )

plot(completeplot)+
  abline(h = 0.67, lty = 2, col = "red" )




#cut the dendrograms into 7 clusters
group1 <- cutree(complete, k=7)



group1 = as.factor(group1)

levels(group1) <- levels(Zoo$type)

confusionMatrix( data = group1 ,reference =Zoo$type)

group2 <-cutree(single,k=7)
group2 = as.factor(group2)

levels(group2) <- levels(Zoo$type)
confusionMatrix(data = group2,reference =  Zoo$type)





#append cluster labels to original data

Zoo = cbind(Zoo,complete_cluster = group1)
Zoo = cbind(Zoo,single_cluster = group2)


