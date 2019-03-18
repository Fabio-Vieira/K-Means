#####################Clustering
#####K-Means
###Simulation

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

absent <- data.frame(read.csv("Absenteeism_at_work.csv", sep = ";", header = T))
head(absent)
plot(absent$Service.time, absent$Absenteeism.time.in.hours)
dt <- scale(cbind(absent$Service.time, absent$Absenteeism.time.in.hours))
fviz_nbclust(dt, FUNcluster = kmeans, method = "silhouette") #with both elbow and silhouette methods

km <- kmeans(dt, centers = 2, nstart = 25)
km$centers
pred <- km$cluster
clusplot(dt, pred, xlab = "Absenteeism", ylab = "Service Time",
         main = "Absenteeism time", lines = 0, shade = T, color = T)
