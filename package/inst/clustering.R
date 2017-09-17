### Clustering

# Data from package cluster

data(ruspini, package="cluster")
plot(ruspini)

# Scale each column in the data to zero mean and unit standard deviation
# (z-scores). This prevents one attribute with a large range to dominate
# the others for the distance calculation.

ruspini_scaled <- scale(ruspini)
plot(ruspini_scaled)

# First, plot a scree plot to determine the amount of clusters

max_clusters <- 15
wss <- (nrow(ruspini_scaled)-1)*sum(apply(ruspini_scaled,2,var))
for (i in 2:max_clusters) wss[i] <- sum(kmeans(ruspini_scaled,centers=i)$withinss)
plot(1:max_clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

### Partitional Clustering - k-means Clustering

km <- kmeans(ruspini_scaled, centers=4, nstart=10)
km
km$centers

plot(ruspini_scaled, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID

### Hierachical clustering

d <- dist(ruspini_scaled, method = "euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)

n_cluster <- 4
groups <- cutree(fit, k=n_cluster)
rect.hclust(fit, k=n_cluster, border="red") 
