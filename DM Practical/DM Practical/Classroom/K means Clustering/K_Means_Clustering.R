iris <- read.csv("C:/Users/DELL/Desktop/SEM-6/DM/Practical2/iris_dirty.csv")

# Load the iris dataset
data(iris)

# Extract the numeric columns
iris_numeric <- iris[,1:4]

# Perform K-Means clustering with k=3
set.seed(123)
kmeans_result <- kmeans(iris_numeric, centers = 3)

# Print the clustering results
print(kmeans_result)

# Plot the data points with different colors indicating their assigned clusters
plot(iris_numeric, col = kmeans_result$cluster)

# Calculate the within-cluster sum of squares (WSS) for different values of k
wss <- c()
for (i in 1:10) {
  set.seed(123)
  kmeans_result <- kmeans(iris_numeric, centers = i)
  wss[i] <- kmeans_result$tot.withinss
}

# Plot the change in WSS as a function of k
plot(1:10, wss, type = "b", xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares")


# FOR WINE
wine <- read.csv("C:/Users/DELL/Desktop/SEM-6/DM/K_Means_Clustering/wine.csv", header=FALSE)
# Load the wine dataset
#data(wine)
wine

# Extract the numeric columns
wine_numeric <- wine[,1:4]

# Perform K-Means clustering with k=3
set.seed(123)
kmeans_result <- kmeans(wine_numeric, centers = 3)

# Print the clustering results
print(kmeans_result)

# Plot the data points with different colors indicating their assigned clusters
plot(wine_numeric, col = kmeans_result$cluster)

# Calculate the within-cluster sum of squares (WSS) for different values of k
wss <- c()
for (i in 1:10) {
  set.seed(123)
  kmeans_result <- kmeans(wine_numeric, centers = i)
  wss[i] <- kmeans_result$tot.withinss
}

# Plot the change in WSS as a function of k
plot(1:10, wss, type = "b", xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares")


