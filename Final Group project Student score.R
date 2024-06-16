all  <- read.csv(file.choose()) 
head(all)

# Load necessary package
library(ggplot2)

# Boxplot of Reading Score by Parents' Education
ggplot(all, aes(x = parents_education, y = reading_score)) +
  geom_boxplot() +
  labs(x = "Parents' Education", y = "Reading Score", title = "Reading Score by Parents' Education") +
  theme_minimal()

# Boxplot of Reading Score by Gender
ggplot(all, aes(x = gender, y = reading_score)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Reading Score", title = "Reading Score by Gender") +
  theme_minimal()

# Scatter Plot of Reading Score vs. Writing Score
ggplot(all, aes(x = reading_score, y = writing_score)) +
  geom_point() +
  labs(x = "Reading Score", y = "Writing Score", title = "Reading vs. Writing Score") +
  theme_minimal()

# Scatter Plot of Reading Score vs. Math Score
ggplot(all, aes(x = reading_score, y = math_score)) +
  geom_point() +
  labs(x = "Reading Score", y = "Math Score", title = "Reading vs. Math Score") +
  theme_minimal()

# Linear model to see the effect of various factors on Reading Score
results <- lm(reading_score ~ gender + race_ethnicity + parents_education + lunch + test_preparation_course, data = all)

# Print the results of the linear model
print(results)

# Summary of the linear model
summary(results)



# Load necessary package
library(ggplot2)

# Assuming the dataset is named `all`
head(all)

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



# Load the data
all <- read.csv(file.choose())  
head(all)

# Plot initial data distributions
qplot(data = all, y = reading_score)   
qplot(data = all, y = writing_score) 
qplot(data = all, y = math_score)


# Create a matrix for clustering
mat <- cbind(all$reading_score, all$writing_score, all$math_score)
head(mat)

# Perform k-means clustering with 3 centers
kmeans_result <- kmeans(mat, centers=3)
km <- kmeans_result$cluster  # vector of cluster belonging

# Add cluster information to the dataset
all$cl <- factor(km)
head(all)

