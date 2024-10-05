# Libraries
library(MASS)  
library(ggplot2)
library(GGally)
library(mclust)
library(kableExtra)

load("wine.rda")

wine_df <- as.data.frame(wine)

load("diabetes.rda")

diabetes_df <- as.data.frame(diabetes)