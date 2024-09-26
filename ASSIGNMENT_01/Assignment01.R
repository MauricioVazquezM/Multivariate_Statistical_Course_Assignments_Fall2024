## Modern Multivariate Statistical Techniques (Izenman) Ex. 8.2
library(MASS)  
library(ggplot2)

# 
load("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_01/wine.rda")

# 
print(wine)

# 
wine_df <- as.data.frame(wine)

# 
head(wine_df)

# Ensure the 'classdigit' column is a factor (if it's not already)
wine_df$classdigit <- as.factor(wine_df$classdigit)

# Perform LDA using the classdigit as the response variable and all other variables as predictors
lda_model <- lda(classdigit ~ Alcohol + MalicAcid + Ash + AlcAsh + Mg + Phenols + Flav + NonFlavPhenols + Proa + Color + Hue + OD + Proline, data = wine_df)

# Project the data onto the LDA axes
lda_values <- predict(lda_model)

# Create a data frame with the LDA components and the wine class
lda_df <- data.frame(LD1 = lda_values$x[, 1], LD2 = lda_values$x[, 2], WineType = wine_df$class)

# Plot the first two discriminant components
ggplot(lda_df, aes(x = LD1, y = LD2, color = WineType)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot of the First Two LDA Components",
       x = "First Discriminant Component (LD1)",
       y = "Second Discriminant Component (LD2)") +
  theme_minimal() +
  theme(legend.title = element_blank())
