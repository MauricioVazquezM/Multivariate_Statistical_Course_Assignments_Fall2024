# Libraries
library(MASS)  
library(ggplot2)
library(GGally)
library(mclust)
library(kableExtra)


###### Ejercicio 1 ######
# Revisando LDA
load("wine.rda")

# Pasando a dataframe
wine_df <- as.data.frame(wine)

# Pasando a variable categorica
wine_df$classdigit <- as.factor(wine_df$classdigit)

# Dividir los datos en entrenamiento (70%) y prueba (30%)
train_indices <- sample(1:nrow(wine_df), 0.7 * nrow(wine_df))

# Conjunto de entrenamiento
train_data <- wine_df[train_indices, ] 

# Conjunto de prueba
test_data <- wine_df[-train_indices, ] 

# LDA 
lda_model <- lda(classdigit ~ Alcohol + MalicAcid + Ash + AlcAsh + Mg + Phenols + Flav + NonFlavPhenols + Proa + Color + Hue + OD + Proline, 
                 data= train_data)

# Proyectando los datos en el LDA
lda_predictions <- predict(lda_model, test_data)

# Craeando dataframe de las componentes
lda_df <- data.frame(LD1 = lda_predictions$x[, 1], LD2 = lda_predictions$x[, 2], WineType =test_data$class)

# Evaluar el modelo con una matriz de confusión
confusion_matrix_lda <- table(Predicted = lda_predictions$class, Actual = test_data$classdigit)
print(confusion_matrix_lda)

# Calcular precisión
accuracy <- sum(diag(confusion_matrix_lda)) / sum(confusion_matrix_lda)
print(paste("Accuracy del modelo LDA:", round(accuracy * 100, 2),"%"))

# Plotting
ggplot(lda_df, aes(x = LD1, y = LD2, color = WineType)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot of the First Two LDA Components",
       x = "First Discriminant Component (LD1)",
       y = "Second Discriminant Component (LD2)") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Cargando data
setwd("~/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/EXAM 01")
load("wine.rda")
wine_df <- as.data.frame(wine)

# Para reproducibilidad
set.seed(123)

# Pasando a variable categorica
wine_df$classdigit <- as.factor(wine_df$classdigit)

# Dividir los datos en entrenamiento (70%) y prueba (30%)
train_indices <- sample(1:nrow(wine_df), 0.7 * nrow(wine_df))

# Conjunto de entrenamiento
train_data <- wine_df[train_indices, ] 

# Conjunto de prueba
test_data <- wine_df[-train_indices, ] 

# Ajustar el modelo QDA 
qda_model_wine <- qda(classdigit ~ Alcohol + MalicAcid + Ash + AlcAsh + Mg + 
                        Phenols + Flav + NonFlavPhenols + Proa + Color + Hue + 
                        OD + Proline, data = train_data)

# Realizar predicciones sobre el cojunto de prueba
qda_predictions <- predict(qda_model_wine, test_data)

# Evaluar el modelo con una matriz de confusión
confusion_matrix <- table(Predicted = qda_predictions$class, Actual = test_data$classdigit)
print(confusion_matrix)

# Calcular precisión
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy del modelo QDA:", round(accuracy * 100, 2),"%"))

# Para visualizar las predicciones
qda_df <- data.frame(LD1 = qda_predictions$x[, 1], LD2 = qda_predictions$x[, 2], 
                     ActualClass = test_data$classdigit, PredictedClass = qda_predictions$class)

# Graficar el scatterplot de las dos primeras componentes discriminantes
ggplot(qda_df, aes(x = LD1, y = LD2, color = PredictedClass, shape = ActualClass)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot de las dos primeras Componentes QDA",
       x = "Primera Componente Discriminante (LD1)",
       y = "Segunda Componente Discriminante (LD2)") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_discrete(name = "Clase Predicha") +
  scale_shape_discrete(name = "Clase Real")

# Calcular F1 score
confusion_stats <- confusionMatrix(lda_predictions$class, test_data$classdigit)
precision <- confusion_stats$byClass["Pos Pred Value"]
recall <- confusion_stats$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))

print(paste("Accuracy del modelo LDA:", round(accuracy * 100, 2),"%"))
prin(paste("F1-Score del modelo LDA:", round(f1_score * 100, 2), "%\n"))




###### Ejercicio 2 ######
load("diabetes.rda")
diabetes_df <- as.data.frame(diabetes)
