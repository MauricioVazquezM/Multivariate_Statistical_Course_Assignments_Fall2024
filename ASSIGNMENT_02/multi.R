#* Tarea 2 - Métodos Multivariados
#* Mariana Luna Rocha
#* Mauricio Vázquez Moran 

library(tidyverse)
library(MASS)
library(ggplot2)
library(GGally)

# ---------------------------- PREGUNTA 7.1 ---------------------------- #

# Paso 1: Configuración y generación de datos
set.seed(42) # Para reproducibilidad
n <- 100
mean <- c(0, 0, 0) # Media de la distribución
cov_matrix <- matrix(c(100, 10, 10, 
                       10, 50, 10, 
                       10, 10, 30), nrow = 3) # Matriz de covarianza

# Generamos los datos aleatorios
data <- mvrnorm(n, mu = mean, Sigma = cov_matrix)
colnames(data) <- c("X1", "X2", "X3")

# Paso 2: PCA con la matriz de covarianza
pca_cov <- prcomp(data, scale. = FALSE) # scale. = FALSE usa la matriz de covarianza
eigenvalues_cov <- pca_cov$sdev^2 # Valores propios
eigenvectors_cov <- pca_cov$rotation # Vectores propios

# Paso 3: PCA con la matriz de correlación
pca_cor <- prcomp(data, scale. = TRUE) # scale. = TRUE usa la matriz de correlación
eigenvalues_cor <- pca_cor$sdev^2 # Valores propios
eigenvectors_cor <- pca_cor$rotation # Vectores propios

# Paso 4: Graficar Scree Plots
par(mfrow=c(1, 2))

# Scree plot para matriz de covarianza
plot(eigenvalues_cov, type="b", xlab="Component", ylab="Eigenvalue",
     main="Scree Plot (Covariance Matrix)")

# Scree plot para matriz de correlación
plot(eigenvalues_cor, type="b", xlab="Component", ylab="Eigenvalue",
     main="Scree Plot (Correlation Matrix)")

# Paso 5: Calcular y graficar los puntajes de las componentes principales
scores_cov <- pca_cov$x # Puntajes de PCA con matriz de covarianza
scores_cor <- pca_cor$x # Puntajes de PCA con matriz de correlación

# Paso 6: Graficar matriz de pares para los puntajes de componentes principales
par(mfrow=c(1, 2))

# Pares de gráficos para matriz de covarianza
pairs(scores_cov, main="Pairwise PC Scores (Covariance Matrix)")

# Pares de gráficos para matriz de correlación
pairs(scores_cor, main="Pairwise PC Scores (Correlation Matrix)")

# Comparación de resultados
cat("Eigenvalues (Covariance Matrix):", eigenvalues_cov, "\n")
cat("Eigenvectors (Covariance Matrix):\n")
print(eigenvectors_cov)

cat("Eigenvalues (Correlation Matrix):", eigenvalues_cor, "\n")
cat("Eigenvectors (Correlation Matrix):\n")
print(eigenvectors_cor)



# ---------------------------- PREGUNTA 7.3 ---------------------------- #
# Paso 1: Cargar y preparar los datos
load("turtles.rda")
turtles$log_length <- log(turtles$length)
turtles$log_width <- log(turtles$width)
turtles$log_height <- log(turtles$height)

# Dividimos los datos entre machos y hembras
male_turtles <- subset(turtles, sex == "m")
female_turtles <- subset(turtles, sex == "f")

# Paso 2: Calcular el vector de medias y la matriz de covarianza para cada grupo
mean_male <- colMeans(male_turtles[, c("log_length", "log_width", "log_height")])
mean_female <- colMeans(female_turtles[, c("log_length", "log_width", "log_height")])

cov_male <- cov(male_turtles[, c("log_length", "log_width", "log_height")])
cov_female <- cov(female_turtles[, c("log_length", "log_width", "log_height")])

# Paso 3: Realizar PCA en cada conjunto de datos
# PCA para tortugas machos
pca_male <- prcomp(male_turtles[, c("log_length", "log_width", "log_height")], scale. = FALSE)
eigenvalues_male <- pca_male$sdev^2
eigenvectors_male <- pca_male$rotation

# PCA para tortugas hembras
pca_female <- prcomp(female_turtles[, c("log_length", "log_width", "log_height")], scale. = FALSE)
eigenvalues_female <- pca_female$sdev^2
eigenvectors_female <- pca_female$rotation

# Paso 4: Calcular el volumen del caparazón
log_volume_male <- rowSums(male_turtles[, c("log_length", "log_width", "log_height")])
log_volume_female <- rowSums(female_turtles[, c("log_length", "log_width", "log_height")])

mean_volume_male <- exp(mean(log_volume_male))
mean_volume_female <- exp(mean(log_volume_female))

# Paso 5: Imprimir resultados y comparar volúmenes
cat("Vector de medias (machos):", mean_male, "\n")
cat("Vector de medias (hembras):", mean_female, "\n")

cat("Matriz de covarianza (machos):\n")
print(cov_male)
cat("Matriz de covarianza (hembras):\n")
print(cov_female)

cat("Valores propios (machos):", eigenvalues_male, "\n")
cat("Vectores propios (machos):\n")
print(eigenvectors_male)

cat("Valores propios (hembras):", eigenvalues_female, "\n")
cat("Vectores propios (hembras):\n")
print(eigenvectors_female)

cat("Volumen promedio del caparazón (machos):", mean_volume_male, "\n")
cat("Volumen promedio del caparazón (hembras):", mean_volume_female, "\n")

# Comparación de volúmenes
cat("Diferencia de volumen entre machos y hembras:", -(mean_volume_male - mean_volume_female), "\n")



# ---------------------------- PREGUNTA 7.10 ---------------------------- #
# Función para construir la matriz de correlación con correlación constante 'p' en entradas fuera de la diagonal
build_correlation_matrix <- function(r, p) {
  matrix <- matrix(p, nrow = r, ncol = r)
  diag(matrix) <- 1
  return(matrix)
}

p_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
r_values <- c(2, 3, 4)

# Cálculo de valores y vectores propios para cada combinación de r y p
for (r in r_values) {
  cat("r =", r, "\n")
  for (p in p_values) {
    R <- build_correlation_matrix(r, p)
    
    eigen_result <- eigen(R)
    eigenvalues <- eigen_result$values
    eigenvectors <- eigen_result$vectors
    
    cat("p =", p, "\n")
    cat("Eigenvalues:\n")
    print(eigenvalues)
    cat("Eigenvectors:\n")
    print(eigenvectors)
    cat("\n")
  }
}

# Observación general
cat("Resultado general:\n")
cat("Un valor propio es 1 + (r-1)*p, con el vector propio (1, 1, ..., 1) / sqrt(r).\n")
cat("Los otros (r-1) valores propios son 1 - p, con vectores propios que suman cero.\n")




# ---------------------------- PREGUNTA 13.1 ---------------------------- #
# Paso 1: Cargar los datos
load("color.stimuli.rda")

# Paso 2: Convertir similitud a distancia
# Supongamos que la similitud máxima es el valor más alto en los datos
max_similarity <- max(color.stimuli)
distance_matrix <- max_similarity - color.stimuli

# Paso 3: Escalamiento clásico 
mds_result <- cmdscale(distance_matrix, k = 2) # Reducción a 2 dimensiones

# Longitudes de onda aproximadas de los colores en nanómetros (nm)
wavelengths <- c("434 nm (púrpura)", "472 nm (azul)", "504 nm (verde)", 
                 "584 nm (amarillo)", "674 nm (rojo)")

# Paso 4: Graficar el círculo de color
plot(mds_result, type = "n", xlab = "Dimensión 1", ylab = "Dimensión 2", main = "Círculo de Color - Escalamiento Clásico")
text(mds_result, labels = wavelengths, col = c("purple", "blue", "green", "yellow", "red"), cex = 1.2)

# Añadir una circunferencia para resaltar la disposición circular (opcional)
symbols(0, 0, circles = max(sqrt(rowSums(mds_result^2))), add = TRUE, inches = FALSE, lty = 2)



# ---------------------------- PREGUNTA 13.2 ---------------------------- #
# Paso 1: Cargar los datos
load("morse.rda")

# Paso 2: Descomposición en parte simétrica y antisimétrica
A <- 0.5 * (morse + t(morse))  # Parte simétrica
B <- 0.5 * (morse - t(morse))  # Parte antisimétrica

#cat("Matriz simétrica A:\n")
#print(A)
#cat("\nMatriz antisimétrica B:\n")
#print(B)

# Paso 3: Escalamiento no métrico en A
# Convertir la matriz de similitudes en una matriz de distancias
# (Suponemos que mayores valores indican mayor similitud)
distance_matrix <- max(A) - A
mds_nonmetric <- cmdscale(as.dist(distance_matrix), k = 2, eig = TRUE, add = TRUE)

# Paso 4: Graficar los resultados 
plot(mds_nonmetric$points, type = "n", xlab = "Dimensión 1", ylab = "Dimensión 2", main = "MDS No Métrica para Morse Code (Parte Simétrica)")
text(mds_nonmetric$points, labels = rownames(morse), cex = 1.2)

# Evaluar el número de dimensiones apropiado
# Podemos revisar los eigenvalores para ver cuánta varianza se explica con cada dimensión
cat("Eigenvalores de la MDS no métrica:\n")
print(mds_nonmetric$eig)



# ---------------------------- PREGUNTA 13.4 ---------------------------- #
# Ejemplo de matriz de disimilitud
A <- matrix(c(0, 1, 2, 1,
              1, 0, 1, 2,
              2, 1, 0, 1,
              1, 2, 1, 0), nrow = 4, byrow = TRUE)

# Construcción de la matriz H
n <- nrow(A)
H <- diag(n) - (1/n) * matrix(1, n, n)

# Matriz doblemente centrada B = HAH
B <- H %*% A %*% H

# Verificar si B es semidefinida positiva
eigenvalues <- eigen(B)$values
cat("Valores propios de B:\n")
print(eigenvalues)
cat("¿B es semidefinida positiva?:", all(eigenvalues >= 0), "\n")



# ---------------------------- PREGUNTA 8.12 ---------------------------- #
# Datos del ejercicio
data <- data.frame(
  Total_population = c(5.935, 1.523, 2.599, 4.009, 4.687, 8.044, 2.766, 6.538, 6.451, 3.314, 3.777, 1.530, 2.768, 6.585),
  Median_school_years = c(14.2, 13.1, 12.7, 15.2, 14.7, 15.6, 13.3, 17.0, 12.9, 12.2, 13.0, 13.8, 13.6, 14.9),
  Total_employment = c(2.265, 0.597, 1.237, 1.649, 2.312, 3.641, 1.244, 2.618, 3.147, 1.606, 2.119, 0.798, 1.336, 2.763),
  Health_services_employment = c(2.27, 0.75, 1.11, 0.81, 2.5, 4.51, 1.03, 2.39, 5.52, 2.18, 2.83, 0.84, 1.75, 1.91),
  Median_value_home = c(2.91, 2.62, 1.72, 3.02, 2.22, 2.36, 1.97, 1.85, 2.01, 1.82, 1.80, 4.25, 2.64, 3.17)
)

# Escalar los datos para el PCA usando la matriz de correlación
scaled_data <- scale(data)

# 1. PCA usando la matriz de correlación
pca_corr <- prcomp(scaled_data, scale = TRUE)
summary(pca_corr)

# Varianza explicada por los componentes principales
var_exp_corr <- pca_corr$sdev^2 / sum(pca_corr$sdev^2)
var_exp_corr

# 2. PCA usando la matriz de covarianza
pca_cov <- prcomp(data, scale = FALSE)
summary(pca_cov)

# Varianza explicada por los componentes principales
var_exp_cov <- pca_cov$sdev^2 / sum(pca_cov$sdev^2)
var_exp_cov

# 3. Visualización de los resultados
# Scree Plot
screeplot(pca_corr, type = "lines", main = "Scree Plot (Correlation Matrix)")
screeplot(pca_cov, type = "lines", main = "Scree Plot (Covariance Matrix)")

# Cargas de los componentes principales
loadings_corr <- pca_corr$rotation
loadings_cov <- pca_cov$rotation

print("Cargas usando matriz de correlación:")
print(loadings_corr)

print("Cargas usando matriz de covarianza:")
print(loadings_cov)



# ---------------------------- PREGUNTA 12.18 ---------------------------- #
# Datos del ejercicio (Tabla 12.12): Matriz de distancias entre ciudades de Wisconsin
distances <- matrix(c(
  0, 107, 127, 141, 129, 230, 232, 210, 340, 180, 334, 325,
  107, 0, 162, 105, 81, 244, 250, 192, 326, 151, 328, 321,
  127, 162, 0, 160, 103, 225, 155, 269, 322, 293, 358, 343,
  141, 105, 160, 0, 57, 266, 251, 181, 300, 95, 278, 261,
  129, 81, 103, 57, 0, 210, 196, 141, 242, 136, 257, 240,
  230, 244, 225, 266, 210, 0, 129, 70, 205, 354, 91, 106,
  232, 250, 155, 251, 196, 129, 0, 199, 105, 324, 53, 68,
  210, 192, 269, 181, 141, 70, 199, 0, 153, 295, 120, 133,
  340, 326, 322, 300, 242, 205, 105, 153, 0, 431, 156, 169,
  180, 151, 293, 95, 136, 354, 324, 295, 431, 0, 389, 368,
  334, 328, 358, 278, 257, 91, 53, 120, 156, 389, 0, 33,
  325, 321, 343, 261, 240, 106, 68, 133, 169, 368, 33, 0
), nrow = 12, ncol = 12, byrow = TRUE)

# Asignamos los nombres de las ciudades
cities <- c("Milwaukee", "Madison", "Green_Bay", "Kenosha", "Racine", "La_Crosse", 
            "Eau_Claire", "Wausau", "Superior", "Beloit", "Rockford", "Chicago")

rownames(distances) <- cities
colnames(distances) <- cities

# 1. Aplicación del MDS para q = 1, 2, 3 dimensiones
mds_1d <- isoMDS(distances, k = 1)
mds_2d <- isoMDS(distances, k = 2)
mds_3d <- isoMDS(distances, k = 3)

# 2. Calcular el stress para q = 1, 2, 3
stress_1d <- mds_1d$stress
stress_2d <- mds_2d$stress
stress_3d <- mds_3d$stress

# Crear un data frame para el gráfico de stress
stress_values <- data.frame(
  q = 1:3,
  stress = c(stress_1d, stress_2d, stress_3d)
)

# Graficar el stress vs. q
ggplot(stress_values, aes(x = q, y = stress)) +
  geom_line() +
  geom_point() +
  labs(title = "Stress vs. Dimensión (q)", x = "Dimensión (q)", y = "Stress") +
  theme_minimal()

# 3. Comparar la configuración bidimensional con el mapa real
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



# ---------------------------- PREGUNTA 12.19 ---------------------------- #
# Datos del ejercicio (Tabla 12.13): Matriz de "distancias" entre sitios arqueológicos
distances <- matrix(c(
  0, 3.4, 2.9, 2.2, 3.8, 4.6, 4.0, 5.7,
  3.4, 0, 1.2, 3.5, 2.1, 4.5, 3.1, 5.8,
  2.9, 1.2, 0, 2.9, 3.1, 4.2, 3.3, 5.5,
  2.2, 3.5, 2.9, 0, 4.4, 5.1, 4.7, 6.3,
  3.8, 2.1, 3.1, 4.4, 0, 2.8, 1.9, 3.4,
  4.6, 4.5, 4.2, 5.1, 2.8, 0, 2.7, 4.1,
  4.0, 3.1, 3.3, 4.7, 1.9, 2.7, 0, 3.2,
  5.7, 5.8, 5.5, 6.3, 3.4, 4.1, 3.2, 0
), nrow = 8, ncol = 8, byrow = TRUE)

# Asignamos los nombres de los sitios arqueológicos
sites <- c("Site1", "Site2", "Site3", "Site4", "Site5", "Site6", "Site7", "Site8")

rownames(distances) <- sites
colnames(distances) <- sites

# 1. Aplicación del MDS para q = 3, 4, 5 dimensiones
mds_3d <- isoMDS(distances, k = 3)
mds_4d <- isoMDS(distances, k = 4)
mds_5d <- isoMDS(distances, k = 5)

# 2. Calcular el stress para q = 3, 4, 5
stress_3d <- mds_3d$stress
stress_4d <- mds_4d$stress
stress_5d <- mds_5d$stress

# Crear un data frame para el gráfico de stress
stress_values <- data.frame(
  q = 3:5,
  stress = c(stress_3d, stress_4d, stress_5d)
)

# Graficar el stress vs. q
ggplot(stress_values, aes(x = q, y = stress)) +
  geom_line() +
  geom_point() +
  labs(title = "Stress vs. Dimensión (q)", x = "Dimensión (q)", y = "Stress") +
  theme_minimal()

# 3. Proyección a dos dimensiones usando PCA de la solución de q = 5
mds_5d_points <- as.data.frame(mds_5d$points)
colnames(mds_5d_points) <- paste0("Dim", 1:5)

# Aplicamos PCA a los puntos en 5 dimensiones
pca_result <- prcomp(mds_5d_points, scale. = TRUE)
pca_2d <- data.frame(pca_result$x[, 1:2])
pca_2d$Site <- sites

# Graficar la configuración bidimensional
ggplot(pca_2d, aes(x = PC1, y = PC2, label = Site)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  labs(title = "Proyección PCA de la Configuración MDS en 2D",
       x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal()

# ---------------------------- PREGUNTA 3.3 ---------------------------- #
# Librerias necesarias para esta pregunta
library(copula)
library(ggplot2)
library(GGally)

# Leyendo datos
wine_data_path <- "C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_02/winequality-red.txt"
wine_data <- read.table(wine_data_path, header = TRUE, sep = ";", quote = "\"")

# Cargar y transformar el conjunto de datos wine_data a pseudo-observaciones
wine_data_pseudo <- as.data.frame(sapply(wine_data, function(x) rank(x) / (length(x) + 1)))

# Quedandonos con 7 columnas
wine_data_subset <- wine_data_pseudo[, c("fixed.acidity", "volatile.acidity", "citric.acid", 
                                         "residual.sugar", "chlorides", "free.sulfur.dioxide", 
                                         "total.sulfur.dioxide")]

# Generar un gráfico cópula
ggpairs(
  wine_data_subset,
  lower = list(continuous = wrap("density", alpha = 0.7, color = "blue")),
  diag = list(continuous = wrap("barDiag", color = "grey", bins = 10)),
  upper = list(continuous = wrap("cor", size = 2, color = "red"))
) + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0) 
  )

# ---------------------------- PREGUNTA 3.4 ---------------------------- #
# Cargando data uranium
data(uranium, package = 'copula')

# Transformar los datos al "copula scale" usando distribuciones empíricas marginales
uranium_pseudo <- as.data.frame(sapply(uranium, function(x) rank(x) / (length(x) + 1)))

# Quedandonos con 3 columnas
uranium_data_subset <- uranium_pseudo[, c("Co", "K","Cs")]

# Grafico copula
ggpairs(
  uranium_data_subset,
  lower = list(continuous = wrap("density", alpha = 0.7, color = "blue")),
  diag = list(continuous = wrap("barDiag", color = "grey", bins = 10)),
  upper = list(continuous = wrap("cor", size = 3, color = "red"))
) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

# 2. Selección de un par de variables para ajustar y comparar (e.g., Cs y Sc)
u <- uranium_pseudo$Cs
v <- uranium_pseudo$Sc

# 3. Ajuste de diferentes cópulas a las pseudo-observaciones
# Ajuste de una cópula Gaussiana
fit_gauss <- fitCopula(normalCopula(dim = 2), cbind(u, v), method = "ml")

# Ajuste de una cópula t-Student
fit_t <- fitCopula(tCopula(dim = 2), cbind(u, v), method = "ml")

# Ajuste de una cópula Clayton
fit_clayton <- fitCopula(claytonCopula(dim = 2), cbind(u, v), method = "ml")

# Ajuste de una cópula Gumbel
fit_gumbel <- fitCopula(gumbelCopula(dim = 2), cbind(u, v), method = "ml")

# 4. Generación de contornos empíricos
empirical_plot <- ggplot(data = data.frame(u, v), aes(x = u, y = v)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  labs(title = "Empirical Contour Plot") +
  theme_minimal()

# 5. Generación de contornos ajustados para cada cópula
# Función auxiliar para generar contornos ajustados
generate_contour_plot <- function(copula_model, title) {
  simulated_data <- rCopula(1000, copula_model@copula)
  ggplot(data = data.frame(simulated_data), aes(x = X1, y = X2)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    labs(title = title) +
    theme_minimal()
}

# Gráficos de contornos ajustados
gauss_plot <- generate_contour_plot(fit_gauss, "Gaussian Copula Contour Plot")
t_plot <- generate_contour_plot(fit_t, "t-Copula Contour Plot")
clayton_plot <- generate_contour_plot(fit_clayton, "Clayton Copula Contour Plot")
gumbel_plot <- generate_contour_plot(fit_gumbel, "Gumbel Copula Contour Plot")

# Mostrar todos los gráficos
library(gridExtra)
grid.arrange(empirical_plot, gauss_plot, t_plot, clayton_plot, gumbel_plot, ncol = 5)

# ---------------------------- PREGUNTA 3.5 ---------------------------- #
# Path
file_path <- "C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS MULTIVARIADOS/REPOSITORIO/Multivariate_Statistical_Course_Assignments_Fall2024/ASSIGNMENT_02/abalone.data"

# Leyendo el archivo
abalone_data <- read.table(file_path, sep = ",", header = FALSE)

# Transformar los datos al "copula scale" usando distribuciones empíricas marginales
abalone_pseudo <- as.data.frame(sapply(abalone_data, function(x) rank(x) / (length(x) + 1)))

# Grafico copula
ggpairs(
  abalone_pseudo,
  lower = list(continuous = wrap("density", alpha = 0.7, color = "blue")),
  diag = list(continuous = wrap("barDiag", color = "grey", bins = 10)),
  upper = list(continuous = wrap("cor", size = 3, color = "red"))
) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

# ---------------------------- PREGUNTA 3.6 ---------------------------- #

# Parámetros a definir
rhos <- c(0.7, -0.2)  # Correlaciones a evaluar
dfs <- seq(2, 30, by = 2)  # Grados de libertad de 2 a 30

# Función para calcular densidad de la t-Copula bivariada
calc_t_copula_density <- function(rho, df) {
  # Crear la copula t bivariada con los parámetros dados
  copula_t <- tCopula(param = rho, df = df, dim = 2)
  
  # Generar un grid de puntos
  u <- seq(0.01, 0.99, length.out = 100)
  v <- seq(0.01, 0.99, length.out = 100)
  
  # Crear un dataframe para almacenar resultados
  results <- expand.grid(u = u, v = v)
  
  # Calcular la densidad para cada par (u, v)
  results$density <- apply(results, 1, function(row) {
    dCopula(c(row["u"], row["v"]), copula = copula_t)
  })
  
  return(results)
}

# Ejemplo de cálculo de densidad para rho = 0.7 y df = 5
example_density <- calc_t_copula_density(rho = 0.7, df = 5)

# Resultados para diferentes grados de libertad
results_list <- list()
for (rho in rhos) {
  for (df in dfs) {
    density_data <- calc_t_copula_density(rho = rho, df = df)
    results_list[[paste0("rho_", rho, "_df_", df)]] <- density_data
  }
}

# El objeto 'results_list' contiene las densidades calculadas para cada combinación
# Ejemplo de cómo acceder a los resultados:
# results_list[["rho_0.7_df_10"]] muestra los valores para rho = 0.7 y df = 10


# ---------------------------- PREGUNTA 3.10 ---------------------------- #

# Librerías necesarias
library(copula)
library(ggplot2)

# Parámetros de Kendall's tau
taus <- c(0.5, 0.8)
u_values <- seq(0.01, 0.99, length.out = 100)

# Función para calcular las funciones h condicionales
calc_conditional_h <- function(tau, fixed_u1 = 0.5, fixed_u2 = 0.5) {
  # Calcular el parámetro theta
  theta <- 2 * tau / (1 - tau)
  
  # Crear la copula Clayton
  clayton_copula <- claytonCopula(theta)
  
  # C_{2|1}(u2 | u1 = fixed_u1)
  h_2_given_1 <- sapply(u_values, function(u2) {
    h <- pCopula(c(fixed_u1, u2), clayton_copula) / fixed_u1
    return(h)
  })
  
  # C_{1|2}(u1 | u2 = fixed_u2)
  h_1_given_2 <- sapply(u_values, function(u1) {
    h <- pCopula(c(u1, fixed_u2), clayton_copula) / fixed_u2
    return(h)
  })
  
  # Resultados
  data.frame(u = u_values, h_2_given_1 = h_2_given_1, h_1_given_2 = h_1_given_2)
}

# Función para graficar
plot_conditional_h <- function(results, tau) {
  p1 <- ggplot(results, aes(x = u)) +
    geom_line(aes(y = h_2_given_1, color = "C_{2|1}(u2 | u1 = 0.5)")) +
    geom_line(aes(y = h_1_given_2, color = "C_{1|2}(u1 | u2 = 0.5)")) +
    labs(title = paste("Conditional h-functions for Clayton Copula with Tau =", tau),
         x = "u", y = "h(u)") +
    scale_color_manual(values = c("blue", "red"), name = "Conditional Functions") +
    theme_minimal()
  print(p1)
}

# Calcular y graficar para tau = 0.5
results_05 <- calc_conditional_h(tau = 0.5)
plot_conditional_h(results_05, tau = 0.5)

# Calcular y graficar para tau = 0.8
results_08 <- calc_conditional_h(tau = 0.8)
plot_conditional_h(results_08, tau = 0.8)

# Análisis para Clayton Copula rotada 90°
rot_clayton <- function(tau, fixed_u1 = 0.5, fixed_u2 = 0.5) {
  theta <- 2 * tau / (1 - tau)
  
  # Crear la copula Clayton rotada 90 grados
  clayton_copula <- rotCopula(claytonCopula(theta), flip = c(TRUE, FALSE))
  
  h_2_given_1 <- sapply(u_values, function(u2) {
    h <- pCopula(c(fixed_u1, u2), clayton_copula) / fixed_u1
    return(h)
  })
  
  h_1_given_2 <- sapply(u_values, function(u1) {
    h <- pCopula(c(u1, fixed_u2), clayton_copula) / fixed_u2
    return(h)
  })
  
  data.frame(u = u_values, h_2_given_1 = h_2_given_1, h_1_given_2 = h_1_given_2)
}

# Graficar para copula rotada con tau = 0.5
results_rot_05 <- rot_clayton(tau = 0.5)
plot_conditional_h(results_rot_05, tau = "0.5 (Rotated)")

# Graficar para copula rotada con tau = 0.8
results_rot_08 <- rot_clayton(tau = 0.8)
plot_conditional_h(results_rot_08, tau = "0.8 (Rotated)")




