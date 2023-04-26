#K-Means
data(iris)
iris_clust <- iris[, -5] # remove species column
iris_clust_scaled <- scale(iris_clust)
set.seed(123)
kmeans_fit <- kmeans(iris_clust_scaled, centers = 3)
table(kmeans_fit$cluster, iris$Species)
library(ggplot2)
iris_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(kmeans_fit$cluster))) +
  geom_point() +
  labs(title = "K-means clustering of iris dataset") +
  scale_color_discrete(name = "Cluster")
print(iris_plot)

#DBScan
data(iris)
iris_clust <- iris[, -5] # remove species column
iris_clust_scaled <- scale(iris_clust)
library(dbscan)
dbscan_fit <- dbscan(iris_clust_scaled, eps = 0.5, minPts = 5)
table(dbscan_fit$cluster, iris$Species)
library(ggplot2)
iris_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(dbscan_fit$cluster))) +
  geom_point() +
  labs(title = "DBSCAN clustering of iris dataset") +
  scale_color_discrete(name = "Cluster")
print(iris_plot)

#Hierarchial Clustering:
data(iris)
iris_clust <- iris[, -5] # remove species column
iris_clust_scaled <- scale(iris_clust)
hclust_fit <- hclust(dist(iris_clust_scaled), method = "ward.D2")
plot(hclust_fit, hang = -1, main = "Dendrogram of hierarchical clustering")
cut_tree_fit <- cutree(hclust_fit, h = 0.7)
table(cut_tree_fit, iris$Species)
library(ggplot2)
iris_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(cut_tree_fit))) +
  geom_point() +
  labs(title = "Hierarchical clustering of iris dataset") +
  scale_color_discrete(name = "Cluster")
print(iris_plot)

