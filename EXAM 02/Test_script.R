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