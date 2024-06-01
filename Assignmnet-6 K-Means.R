all  <- read.csv(file.choose())
head(iris)

# Plot the Sepal Length vs Sepal Width
qplot(data=iris, x=Sepal.Length, y=Sepal.Width, color=Species)

# Convert the relevant features to a matrix
mat <- as.matrix(iris[, 1:4])
head(mat)

kmeans_result <- kmeans(mat, centers=3)

# Add the cluster assignment to the iris dataframe
iris$cluster <- as.factor(kmeans_result$cluster)

# View the first few rows of the updated iris dataframe
head(iris)

# Summary of the clustering result
summary(iris)

# Plot Sepal Length vs Sepal Width with cluster coloring using qplot
qplot(data=iris, x=Sepal.Length, y=Sepal.Width, color=cluster, main="Sepal Length vs Sepal Width by Cluster")

# Plot Petal Length vs Petal Width with cluster coloring using qplot
qplot(data=iris, x=Petal.Length, y=Petal.Width, color=cluster, main="Petal Length vs Petal Width by Cluster")




# Load the necessary libraries
install.packages("plot3D")
library(plot3D)

# Load the Iris dataset
all  <- read.csv(file.choose())
head(iris)

# Convert the relevant features to a matrix
mat <- as.matrix(iris[, 1:4])

# Perform k-means clustering with multiple centers to find the optimal number of clusters using the elbow method
wss <- rep(0, 15)  # Initialize the within-cluster sum of squares vector

for (k in 1:15) {
  wss[k] <- sum(kmeans(mat, centers=k, nstart=50)$withinss)
}

# Plot the within-cluster sum of squares to find the elbow point
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within-cluster sum of squares", main="Elbow Method for Optimal Clusters")

# Perform k-means clustering with 3 centers as indicated by the elbow method
kmeans_result <- kmeans(mat, centers=3)

# Add the cluster assignment to the iris dataframe
iris$cluster <- as.factor(kmeans_result$cluster)

# View the first few rows of the updated iris dataframe
head(iris)

# 3D Scatter plot of the clustering result using Sepal.Length, Sepal.Width, and Petal.Length
scatter3D(x=iris$Sepal.Length, y=iris$Sepal.Width, z=iris$Petal.Length, colvar=as.integer(iris$cluster), pch=19, colkey=FALSE, main="3D Scatter Plot of Iris Clusters", xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")

# 3D Scatter plot of the clustering result using Petal.Length, Petal.Width, and Sepal.Length
scatter3D(x=iris$Petal.Length, y=iris$Petal.Width, z=iris$Sepal.Length, colvar=as.integer(iris$cluster), pch=19, colkey=FALSE, main="3D Scatter Plot of Iris Clusters", xlab="Petal Length", ylab="Petal Width", zlab="Sepal Length")




# Load the Iris dataset
all  <- read.csv(file.choose())

# Display the first few rows of the Iris dataset
head(iris)

# Plotting Internet users from the Iris dataset
qplot(data = iris, y = Petal.Width)  # All over the place!!

# Perform k-means clustering on Petal.Width
kmeans_result <- kmeans(iris$Petal.Width, centers = 3)

# Let's draw it
km <- kmeans_result$cluster  # vector of cluster belonging
iris$cl <- factor(km)  
qplot(data = iris, y = Petal.Width, color = Species)
qplot(data = iris, y = Petal.Width, color = cl)

# Plotting Birth rate from the Iris dataset
qplot(data = iris, y = Sepal.Length)  # All over the place!!

# Perform k-means clustering on Sepal.Length
kmeans_result <- kmeans(iris$Sepal.Length, centers = 3)

# Let's draw it
km <- kmeans_result$cluster  # vector of cluster belonging
iris$cl <- factor(km)  
qplot(data = iris, y = Sepal.Length, color = Species)
qplot(data = iris, y = Sepal.Length, color = cl)
