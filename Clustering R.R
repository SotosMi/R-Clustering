#Topic 2

library(cluster)


wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = FALSE)


# Naming our atributes

colnames(wine) <- c('cultivar', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')



wines = wine[,-1] # first column is a class label 


wine_scaled <- as.data.frame(scale(wines))
colMeans(wine_scaled) #means = almost 0
apply(wine_scaled, 2, sd) # sd = 1

set.seed(100)

#Applying the K-means algorithm

wines_k <- kmeans(wine_scaled, centers=3)

cen <- as.data.frame(wines_k$centers) # centers of the centroids of each cluster


#Provide the Euclidean distances between the different cluster centroids

distance <- get_dist(cen)
distance 

#Create a new column cluster with the assigned cluster for each row

wine <- data.frame(wine,cluster = as.factor(wines_k$cluster))

#Count how many wines are assigned to each cluster and compare it with the attribute cultivar
tab <- table(wine$cultivar,wine$cluster)
tab

#Run the k-means for different k parameter and visualize the diagram of within cluster sum of squares for each k.
#Find the optimal number of clusters using the elbow rule and explain your answer.


#The quality of a k-means partition is found by calculating the percentage  
#for Between Sum of Squares divided Total Sum of Squares, respectively. 
#The higher the percentage, the better the score (and thus the quality of k)
# because it means that Between Sum of Squares is large.

BSS <- wines_k$betweenss
TSS <- wines_k$totss


# We calculate the quality of the partition for K = 3
BSS / TSS * 100

# loop to store the total within clusters sum of squares
# so we can run it for multiple k

bss<- numeric()
wss<-numeric()

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(wine_scaled, centers=i)$betweenss
  wss[i] <- kmeans(wine_scaled, centers=i)$tot.withinss
  
}

library(ggplot2)

plot_ofsquares <- qplot(1:10, wss, geom=c("point", "line"),
                        xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) + 
  geom_vline(xintercept =3, linetype=5, col= "darkred") + # add line for better visualisation
  labs(subtitle = "Elbow method")+ # add subtitle
  theme_bw()

plot_ofsquares


