{r}
all <- read.csv("StudentGrades.csv") 
head(all)

missing_values <- colSums(is.na(all))
missing_values

data_types <- sapply(all, class)
data_types

unique_values <- sapply(all, function(x) length(unique(x)))
unique_values

summary(all)

library(ggplot2)
library(plot3D)
library(dplyr)

ggplot(all, aes(x = reading_score)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Reading Scores", x = "Reading Score", y = "Density") +
  theme_minimal()

ggplot(all, aes(x = reading_score)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.5) +
  labs(title = "Histogram of writing scores", x = "writing score", y = "Frequency") +
  theme_minimal()

ggplot(all, aes(x = gender, y = reading_score)) +
  geom_boxplot(fill = c("orange", "purple"), color = "black") +
  labs(title = "Box-and-Whisker Plot of Reading Scores by Gender", x = "Gender", y = "Reading Score") +
  theme_minimal()

ggplot(all, aes(x = math_score, y = reading_score)) +
  geom_point(aes(color = gender), size = 3) +
  labs(title = "Scatterplot of Math Score vs. Reading Score", x = "Math Score", y = "Reading Score") +
  theme_minimal()

# Create a matrix for clustering
mat <- cbind(all$reading_score, all$math_score)
head(mat)

# Perform k-means clustering with 3 centers
kmeans_result <- kmeans(mat, centers=3)
km <- kmeans_result$cluster  # vector of cluster belonging

# Add cluster information to the dataset
all$cl <- factor(km)

# Scatter plot of Reading Score vs. Math Score with clusters
ggplot(all, aes(x = reading_score, y = math_score, color = cl)) +
  geom_point() +
  labs(x = "Reading Score", y = "Math Score", title = "Reading Score vs. Math Score with Clusters") +
  theme_minimal()

# Create a matrix for clustering
mat <- cbind(all$reading_score, all$writing_score)
head(mat)

# Perform k-means clustering with 3 centers
kmeans_result <- kmeans(mat, centers=3)
km <- kmeans_result$cluster  # vector of cluster belonging

# Add cluster information to the dataset
all$cl <- factor(km)

# Scatter plot of Reading Score vs. Writing Score with clusters
ggplot(all, aes(x = reading_score, y = writing_score, color = cl)) +
  geom_point() +
  labs(x = "Reading Score", y = "Writing Score", title = "Reading Score vs. Writing Score with Clusters") +
  theme_minimal()