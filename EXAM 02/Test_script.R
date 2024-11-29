###### Libraries ######
library(MASS)  
library(ggplot2)
library(GGally)
library(mclust)
library(kableExtra)
library(FactoMineR)
library(knitr)
library(caret)
library(dplyr)
library(gridExtra)
library(ggfortify)
library(copula)


 


###### Pregunta 1 ######

# Ruta al archivo
file_path <- "C:\\Users\\mauva\\OneDrive\\Documents\\ITAM\\9no Semestre\\METODOS MULTIVARIADOS\\REPOSITORIO\\Multivariate_Statistical_Course_Assignments_Fall2024\\EXAM 02\\turtles.rda"

# Carga de los datos
load(file_path)

# Revision de los datos
head(turtles)

# Analisis univariado
univar_analisis <- function(data) {
  results <- list()
  
  for (feature in colnames(data)) {
    data_type <- class(data[[feature]])[1]
    
    total <- nrow(data)
    nan_count <- sum(is.na(data[[feature]]))
    no_missings <- total - nan_count
    pct_missings <- nan_count / total
    
    if (is.numeric(data[[feature]])) {
      promedio <- round(mean(data[[feature]], na.rm = TRUE),2)
      desv_estandar <- round(sd(data[[feature]], na.rm = TRUE),2)
      varianza <- round(var(data[[feature]], na.rm = TRUE),2)
      minimo <- min(data[[feature]], na.rm = TRUE)
      p10 <- quantile(data[[feature]], 0.10, na.rm = TRUE)
      q1 <- quantile(data[[feature]], 0.25, na.rm = TRUE)
      mediana <- quantile(data[[feature]], 0.50, na.rm = TRUE)
      q3 <- quantile(data[[feature]], 0.75, na.rm = TRUE)
      p90 <- quantile(data[[feature]], 0.90, na.rm = TRUE)
      p95 <- quantile(data[[feature]], 0.95, na.rm = TRUE)
      p99 <- quantile(data[[feature]], 0.99, na.rm = TRUE)
      maximo <- max(data[[feature]], na.rm = TRUE)
      
      inf_count <- sum(is.infinite(data[[feature]]) & data[[feature]] > 0)
      neg_inf_count <- sum(is.infinite(data[[feature]]) & data[[feature]] < 0)
    } else {
      promedio <- NA
      desv_estandar <- NA
      varianza <- NA
      minimo <- NA
      p1 <- NA
      p5 <- NA
      p10 <- NA
      q1 <- NA
      mediana <- NA
      q3 <- NA
      p90 <- NA
      p95 <- NA
      p99 <- NA
      maximo <- NA
      inf_count <- 0
      neg_inf_count <- 0
    }
    
    results[[length(results) + 1]] <- list(
      
      Variable = feature,
      Total = total,
      No_Missings = no_missings,
      Missings = nan_count,
      Pct_Missings = pct_missings,
      Promedio = promedio,
      Desv_Std = desv_estandar,
      Varianza = varianza,
      Minimo = minimo,
      p10 = p10,
      q1 = q1,
      Mediana = mediana,
      q3 = q3,
      p90 = p90,
      p95 = p95,
      p99 = p99,
      Maximo = maximo
    )
  }
  
  result_df <- do.call(rbind, lapply(results, as.data.frame))
  
  rownames(result_df) <- NULL
  
  return(result_df)
  
}

# Ejecucion de lafuncion
resultados <- univar_analisis(turtles)

# Separando el analisis
resultados_parte1 <- resultados[, c("Variable", "Total", "No_Missings", "Missings", "Pct_Missings", "Promedio", "Desv_Std")]

resultados_parte2 <- resultados[, c("Variable", "Varianza", "Minimo", "p10", "q1", "Mediana", "q3", "p90", "p95", "p99", "Maximo")]

# Cantidades por sexo
sex_counts <- table(turtles$sex)

# Visualización: Gráfico de barras
barplot(
  sex_counts,
  main = "Cantidad de Tortugas por Sexo",
  xlab = "Sexo",
  ylab = "Cantidad",
  col = c("lightblue", "lightpink"),
  border = "black",
  ylim = c(0, max(sex_counts) + 5) 
)

# Crear el histograma 
histograma <- ggplot(turtles, aes(x = length)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histograma de las longitudes de las tortugas", x = "length", y = "Frecuencia") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Crear la curva de densidad 
densidad <- ggplot(turtles, aes(x = length)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Curva de densidad las longitudes de las tortugas", x = "length", y = "Densidad") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Mostrar ambos gráficos en un solo layout
grid.arrange(histograma, densidad, ncol = 2)

# Crear el histograma 
histograma_w <- ggplot(turtles, aes(x = width)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de lo ancho de las tortugas", x = "width", y = "Frecuencia") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Crear la curva de densidad 
densidad_w <- ggplot(turtles, aes(x = width)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Curva de densidad de lo ancho de las tortugas", x = "width", y = "Densidad") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Mostrar ambos gráficos en un solo layout
grid.arrange(histograma_w, densidad_w, ncol = 2)

# Crear el histograma 
histograma_h <- ggplot(turtles, aes(x = heigth)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la altura de las tortugas", x = "height", y = "Frecuencia") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Crear la curva de densidad 
densidad_h <- ggplot(turtles, aes(x = heigth)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Curva de densidad de la altura de las tortugas", x = "height", y = "Densidad") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 9),        
    axis.title = element_text(size = 8),         
    axis.text = element_text(size = 7)           
  )

# Mostrar ambos gráficos en un solo layout
grid.arrange(histograma_h, densidad_h, ncol = 2)

# Boxplots agregados por sexo
box_l <- ggplot(turtles, aes(x = sex, y = length, fill = sex)) +
  geom_boxplot() +
  labs(title = "Longitud por Sexo", x = "Sexo", y = "Length") +
  theme_minimal()

box_w <- ggplot(turtles, aes(x = sex, y = width, fill = sex)) +
  geom_boxplot() +
  labs(title = "Ancho por Sexo", x = "Sexo", y = "Width") +
  theme_minimal()

box_h <- ggplot(turtles, aes(x = sex, y = height, fill = sex)) +
  geom_boxplot() +
  labs(title = "Altura por Sexo", x = "Sexo", y = "Height") +
  theme_minimal()

# Mostrar ambos gráficos en un solo layout
grid.arrange(box_l, box_w, box_h, ncol = 2)

# Dataset reducido
numerical_vars <- turtles[, c("length", "width", "height")]

# Creando el pairs plot 
ggpairs(
  numerical_vars,
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag"), 
  upper = list(continuous = "cor"),    
  aes(color = turtles$sex, alpha = 0.7) 
) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"), 
    axis.title = element_text(size = 12),               
    axis.text = element_text(size = 10),                
    legend.position = "top"                             
  ) +
  labs(title = "Analisis de correlación (segmentado por sexo)", color = "Sexo")

# Variables numéricas
numerical_vars <- turtles[, c("length", "width", "height")]

# PCA
pca_result <- prcomp(numerical_vars, scale. = TRUE)

# Resumen del PCA
#summary(pca_result)

# Añadir la variable 'sex' para colorear por sexo
projected_data <- data.frame(pca_result$x, sex = turtles$sex)

# Gráfico
ggplot(projected_data, aes(x = PC1, y = PC2, color = sex)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Proyección a las 2 primeras Componentes Principales",
    x = "Primera Componente Principal (PC1)",
    y = "Segunda Componente Principal (PC2)",
    color = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )





###### Pregunta 2 ######
set.seed(42) 
# Parámetro de theta
theta <- 2  

# Cópula de Clayton
clayton_copula <- claytonCopula(param = theta, dim = 2)

# Simulando datos
n <- 1000  
simulated_data <- rCopula(n, clayton_copula)

# Pasando a un dataframe
data <- data.frame(x = simulated_data[, 1], y = simulated_data[, 2])

# Graficando
ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_density_2d(color = "red") + 
  labs(
    title = "Simulación de una Cópula Clayton",
    x = "Variable 1",
    y = "Variable 2"
  ) +
  theme_minimal()

# Curvas de contorno
ggplot(data, aes(x = x, y = y)) +
  geom_density_2d_filled(contour_var = "density", alpha = 0.6) +
  labs(title = "Curvas de Contorno - Cópula de Clayton",
       x = "Variable 1",
       y = "Variable 2") +
  theme_minimal()

# Mostrar ambos gráficos en un solo layout
#grid.arrange(a, b, ncol = 2)



###### Pregunta 3 ######
# Matriz de distancias
distances <- matrix(c(
  0, 130, 98, 102, 103, 100, 149, 315, 91, 196, 257, 186,
  130, 0, 33, 50, 185, 73, 33, 377, 186, 94, 304, 97,
  98, 33, 0, 36, 164, 54, 58, 359, 166, 119, 287, 113,
  102, 50, 36, 0, 138, 77, 47, 330, 139, 95, 258, 146,
  103, 185, 164, 138, 0, 184, 170, 219, 45, 186, 161, 276,
  100, 73, 54, 77, 184, 0, 107, 394, 181, 168, 322, 93,
  149, 33, 58, 47, 170, 107, 0, 362, 186, 61, 289, 130,
  315, 377, 359, 330, 219, 394, 362, 0, 223, 351, 162, 467,
  91, 186, 166, 139, 45, 181, 186, 223, 0, 215, 175, 275,
  196, 94, 119, 95, 186, 168, 61, 351, 215, 0, 274, 184,
  257, 304, 287, 258, 161, 322, 289, 162, 175, 274, 0, 395,
  186, 97, 113, 146, 276, 93, 130, 467, 275, 184, 395, 0
), nrow = 12, byrow = TRUE)

# Añadir nombres de filas y columnas
cities <- c(
  "Appleton", "Beloit", "Fort Atkinson", "Madison", "Marshfield", 
  "Milwaukee", "Monroe", "Superior", "Wausau", "Dubuque", "St. Paul", "Chicago"
)
colnames(distances) <- rownames(distances) <- cities

# MDS para 1, 2, 3 dimensiones
mds_1d <- isoMDS(distances, k = 1)
mds_2d <- isoMDS(distances, k = 2)
mds_3d <- isoMDS(distances, k = 3)

# Configuración bidimensional con el mapa real
mds_2d_points <- data.frame(mds_2d$points)
colnames(mds_2d_points) <- c("Dim1", "Dim2")
mds_2d_points$City <- cities

# Graficar la configuración bidimensional
ggplot(mds_2d_points, aes(x = Dim1, y = Dim2, label = City)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  labs(title = "Configuración MDS Bidimensional de Ciudades",
       x = "Dimensión 1", y = "Dimensión 2") +
  theme_minimal()
