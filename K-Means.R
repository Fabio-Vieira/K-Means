#####################Clustering
#####K-Means
###A simple simulation to make me believe k-means really work

data <- data.frame(A = rnorm(100, mean = c(-3, 0, 3)),
			 B = rnorm(100, mean = c(10, 0, 3)),
			 C = rnorm(100, mean = c(5, 8, 3))
			)

plot(data, col = 1:3)

centroid <- matrix(c(-3, 0, 3, 10, 0, 3, 5, 8, 3), nrow = 3, ncol = 3)

km <- kmeans(x = data, centers = centroid, nstart = 25)

#Cluster centers
km$centers #it is almost the same as the matrix centroid

#############################################################################
#Exploring Absenteeism data from UCI repo
library(factoextra)
library(cluster)

#Preparing the data
absent <- data.frame(read.csv("Absenteeism_at_work.csv", sep = ";", header = T))
head(absent)
plot(absent$Service.time, absent$Absenteeism.time.in.hours)
dt <- scale(cbind(absent$Service.time, absent$Absenteeism.time.in.hours))

#Detecting the optimal number of clusters
fviz_nbclust(dt, FUNcluster = kmeans, method = "silhouette") 

#by setting nstart as 25, the algorithm chooses different random initial centroids 25 times and only returns "the best one
km <- kmeans(dt, centers = 2, nstart = 25) 
km$centers
pred <- km$cluster

#Plot to see the clusters
clusplot(dt, pred, xlab = "Absenteeism", ylab = "Service Time",
         main = "Absenteeism time", lines = 0, shade = T, color = T)
