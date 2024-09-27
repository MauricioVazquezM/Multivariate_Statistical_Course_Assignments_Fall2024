## Modern Multivariate Statistical Techniques (Izenman) Ex. 8.2
library(MASS)  
library(ggplot2)
library(GGally)

# Loading dataset
load("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_01/wine.rda")

# Parsing to dataframe
wine_df <- as.data.frame(wine)

# Checking head dataset
head(wine_df)

# Parsing dataset column 
wine_df$classdigit <- as.factor(wine_df$classdigit)

# Performing LDA 
lda_model <- lda(classdigit ~ Alcohol + MalicAcid + Ash + AlcAsh + Mg + Phenols + Flav + NonFlavPhenols + Proa + Color + Hue + OD + Proline, data = wine_df)

# Projecting the data onto the LDA axes
lda_values <- predict(lda_model)

# Creating a data frame with the LDA components
lda_df <- data.frame(LD1 = lda_values$x[, 1], LD2 = lda_values$x[, 2], WineType = wine_df$class)

# Plotting
ggplot(lda_df, aes(x = LD1, y = LD2, color = WineType)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot of the First Two LDA Components",
       x = "First Discriminant Component (LD1)",
       y = "Second Discriminant Component (LD2)") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Loading dataset
load("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_01/diabetes.rda")

# Parsing to dataframe
diabetes_df <- as.data.frame(diabetes)

# Checking head dataset
head(diabetes_df)

# Drawing scatterplot matrix for the five predictor variables with color distinction for 'class'
ggpairs(diabetes_df, columns = 1:5, aes(color = as.factor(class), shape = as.factor(class))) +
  theme_minimal() +
  labs(title = "Scatterplot Matrix of Diabetes Data",
       color = "Class", shape = "Class")

# Performing LDA
lda_model <- lda(class ~ glucose.area + insulin.area + SSPG + relative.weight + fasting.plasma.glucose, data = diabetes_df)

# Predicting the discriminant functions
lda_values <- predict(lda_model)

# Creating 2D scatter plot for the first two discriminant functions
lda_df <- data.frame(lda_values$x, class = diabetes_df$class)

ggplot(lda_df, aes(x = LD1, y = LD2, color = as.factor(class), shape = as.factor(class))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "LDA: First Two Discriminant Functions",
       color = "Class", shape = "Class") +
  xlab("LD1") + ylab("LD2")

# Performing LDA with Leave-One-Out CV
lda_cv <- lda(class ~ glucose.area + insulin.area + SSPG + relative.weight + fasting.plasma.glucose, 
              data = diabetes_df, CV = TRUE)

# Confusion table
lda_pred <- lda_cv$class
table(Predicted = lda_pred, Actual = diabetes_df$class)

# Identifying misclassified observations
misclassified_lda <- which(lda_pred != diabetes_df$class)
misclassified_lda 

# Performing QDA
qda_model <- qda(class ~ glucose.area + insulin.area + SSPG + relative.weight + fasting.plasma.glucose, data = diabetes_df)

# Performing QDA with Leave-One-Out CV
qda_cv <- qda(class ~ glucose.area + insulin.area + SSPG + relative.weight + fasting.plasma.glucose, 
              data = diabetes_df, CV = TRUE)

# Confusion table
qda_pred <- qda_cv$class
table(Predicted = qda_pred, Actual = diabetes_df$class)

# Identifying misclassified observations
misclassified_qda <- which(qda_pred != diabetes_df$class)
misclassified_qda

# Loading dataset
load("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_01/wine.rda")

# Parsing to dataframe
wine_df <- as.data.frame(wine)

# Checking head dataset
head(wine_df)

# Compute distance matrix
dist_matrix <- dist(wine_df)

# Single linkage clustering
single_linkage <- hclust(dist_matrix, method = "single")

# Average linkage clustering
average_linkage <- hclust(dist_matrix, method = "average")

# Complete linkage clustering
complete_linkage <- hclust(dist_matrix, method = "complete")

dev.off()
plot(single_linkage, main = "Single Linkage", sub = "", xlab = "", ylab = "Height")
plot(average_linkage, main = "Average Linkage", sub = "", xlab = "", ylab = "Height")
plot(complete_linkage, main = "Complete Linkage", sub = "", xlab = "", ylab = "Height")

load("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_01/primate.scapulae.rda")

df <- as.data.frame(primate.scapulae)

# Computing distance matrix
dist_matrix <- dist(df[, -which(names(df) == "class")])

# Clustering with different linkage methods
single_linkage <- hclust(dist_matrix, method = "single")
average_linkage <- hclust(dist_matrix, method = "average")
complete_linkage <- hclust(dist_matrix, method = "complete")

# Cutting the dendrogram to form 5 clusters
single_clusters <- cutree(single_linkage, k = 5)
average_clusters <- cutree(average_linkage, k = 5)
complete_clusters <- cutree(complete_linkage, k = 5)

# Calculating Adjusted Rand Index (ARI) for each method
ari_single <- adjustedRandIndex(primate.scapulae$class, single_clusters)
ari_average <- adjustedRandIndex(primate.scapulae$class, average_clusters)
ari_complete <- adjustedRandIndex(primate.scapulae$class, complete_clusters)

# Misclassification rates 
misclassification_single <- 1 - ari_single
misclassification_average <- 1 - ari_average
misclassification_complete <- 1 - ari_complete

# Displaying misclassification rates
cat("Misclassification rate (Single-linkage): ", misclassification_single, "\n")
cat("Misclassification rate (Average-linkage): ", misclassification_average, "\n")
cat("Misclassification rate (Complete-linkage): ", misclassification_complete, "\n")

# Create the table of presidents with their characteristics
presidents <- data.frame(
  Name = c("R. Reagan", "J. Carter", "G. Ford", "R. Nixon", "L. Johnson", "J. Kennedy"),
  Birthplace = c("No South", "South", "No South", "No South", "South", "No South"),
  ElectedFirstTerm = c(1, 1, 0, 0, 0, 1),
  Party = c("Republican", "Democrat", "Republican", "Republican", "Democrat", "Democrat"),
  CongressionalExperience = c(0, 0, 1, 1, 1, 1),
  ServedVP = c(0, 0, 1, 0, 1, 0)
)

# Convert the categorical variables to binary form
presidents$BirthplaceBinary <- ifelse(presidents$Birthplace == "South", 1, 0)
presidents$PartyBinary <- ifelse(presidents$Party == "Republican", 1, 0)

# Final binary matrix of variables
binary_data <- presidents[, c("BirthplaceBinary", "ElectedFirstTerm", "PartyBinary", "CongressionalExperience", "ServedVP")]
binary_data

# Function to calculate Jaccard similarity
jaccard_similarity <- function(x, y) {
  return(sum(x == y) / length(x))
}

# Function to calculate Hamming distance
hamming_distance <- function(x, y) {
  return(sum(x != y))
}

# Function to calculate Dice similarity
dice_similarity <- function(x, y) {
  return(2 * sum(x == y) / (length(x) + sum(x == y)))
}

# Calculate similarities for all pairs of presidents
n <- nrow(binary_data)
similarities_jaccard <- matrix(0, n, n)
similarities_dice <- matrix(0, n, n)
hamming_distances <- matrix(0, n, n)

# Loop through all pairs of presidents
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    similarities_jaccard[i, j] <- jaccard_similarity(binary_data[i, ], binary_data[j, ])
    similarities_dice[i, j] <- dice_similarity(binary_data[i, ], binary_data[j, ])
    hamming_distances[i, j] <- hamming_distance(binary_data[i, ], binary_data[j, ])
  }
}

# Display the similarity matrices
cat("Jaccard Similarity Matrix:\n")
print(similarities_jaccard)
cat("Dice Similarity Matrix:\n")
print(similarities_dice)
cat("Hamming Distance Matrix:\n")
print(hamming_distances)

# Convert the matrices into ordered lists to compare the ranks of the coefficients
jaccard_list <- sort(similarities_jaccard[upper.tri(similarities_jaccard)], decreasing = TRUE)
dice_list <- sort(similarities_dice[upper.tri(similarities_dice)], decreasing = TRUE)
hamming_list <- sort(hamming_distances[upper.tri(hamming_distances)])

# Display the ordered coefficients
cat("Jaccard Similarities (Ordered):\n")
print(jaccard_list)
cat("Dice Similarities (Ordered):\n")
print(dice_list)
cat("Hamming Distances (Ordered):\n")
print(hamming_list)

# Creating the dataset with items A, B, C, and D
data <- data.frame(
  Item = c("A", "B", "C", "D"),  
  X1 = c(5, 1, -1, 3),           
  X2 = c(4, -2, 1, 1)            
)

# Displaying the dataset
print(data)

# Defining the initial positions of the cluster centers
initial_centers <- data.frame(
  X1 = c(mean(c(5, 1)), mean(c(-1, 3))),  
  X2 = c(mean(c(4, -2)), mean(c(1, 1)))   
)

# Displaying the initial centers
print(initial_centers)

# Performing k-means clustering using the initial centers
set.seed(123)  
kmeans_result <- kmeans(data[, c("X1", "X2")], centers = initial_centers)  

# Displaying the clustering results
cat("Cluster assignments:\n")  
print(kmeans_result$cluster)

cat("\nCluster centers:\n") 
print(kmeans_result$centers)

d7 <- eurodist
d7 <- na.omit(d7)
kable(head(as.matrix(d7), c(7L, 7L)), row.names = T)

diss_matrix7 <- dist(d7, method="euclidean")
hc1_7 <- hclust(diss_matrix7, method="single")
hc2_7 <- hclust(diss_matrix7, method="complete")
hc3_7 <- hclust(diss_matrix7, method="average")

plot(hc1_7, cex=0.6, hang=-1,
     main = "Cluster Dendrogram Single-Linkage",
     sub = "Matriz de distancia Euclidiana sobre dataset eurodist", las=2)

plot(hc2_7, cex=0.6, hang=-1,
     main = "Cluster Dendrogram Complete-Linkage",
     sub = "Matriz de distancia Euclidiana sobre dataset eurodist", las =2)

plot(hc3_7, cex=0.6, hang=-1,
     main = "Cluster Dendrogram Average-Linkage",
     sub = "Matriz de distancia Euclidiana sobre dataset eurodist", las =2)

