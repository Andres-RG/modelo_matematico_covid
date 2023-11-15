# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)
library(pheatmap)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se carga la base de datos
load("03_Out/OutData/probabilidades_de_transicion.RData")

# Como uno de los primeros análisis para determinar los grupos etarios que
# definirán el modelo, se encuentra un análisis de correlación a partir de las
# probabilidades de transicion obtenidas, mediante la visualizacion con el uso
# de un heatmap. 
heatmap(cor(t(probabilidades_de_transicion)))
etiquetas <- c("18-","18-29","30-39","40-49","50-59","60-69","70+")
heatmap <- pheatmap(
  cor(t(probabilidades_de_transicion)),
  color = viridis(100),
  cellwidth = 75,  # Ancho de las celdas
  cellheight = 75,  # Alto de las celdas
  cluster_rows = F,  # Agrupar filas
  cluster_cols = F,  # Agrupar columnas
  border_color = NA,  # Color del borde
  labels_row = etiquetas,  # Cambiar etiquetas de filas
  labels_col = etiquetas,  # Cambiar etiquetas de columnas
  main = "Correlación p de transición",
  fontsize = 13,  # Tamaño de fuente para etiquetas
  fontsize_row = 12,  # Tamaño de fuente para etiquetas de filas
  fontsize_col = 12,  # Tamaño de fuente para etiquetas de columnas
  angle_col = 0,
)
# Con la visuaclización de las correlaciones entre las probabilidades de transicion,
# se pueden ir separando diferentes grupos que son los que más se correlacionan.
# Estos grupos son: 
# ----------------- 30 - 39 & 18 - 29 (Grupo 1)
# ----------------- 60 - 69 & 70 + (Grupo 2)
# ----------------- 40 - 49 (Grupo 3)
# ----------------- 50 - 59 (Grupo 4)
# ----------------- 18 -  (Grupo 5)

# El objeto que contiene el heatmap se guarda como un archivo png
# jpeg("03_Out/Plots/pretty_heatmap_probabilidades_de_transicion.jpeg",
#      width = 5733, height = 5733, res = 600, units = "px")
# heatmap
# dev.off()

# Como un segundo análisis de clasificación, se realiza un análisis de 
# clasificacion con diferentes algortimos para probar la robustez de la 
# clasificación y tener mayor certeza de que los grupos se definen 
# adecuadamente. 

# Para este análisis, necesitamos primero obtener las distancias entre cada
# probabilidad, para que aquellas probabilidades que tengan menor distancia entre
# ellas sean la que se agrupen.
distancias_pt <- dist(probabilidades_de_transicion)

# Posteriormente, se prueban los diferentes algoritmos de agrupamiento
agroup1 <- hclust(distancias_pt, method = "complete", members = NULL)
agroup2 <- hclust(distancias_pt, method = "ward.D", members = NULL)
agroup3 <- hclust(distancias_pt, method = "ward.D2", members = NULL)
agroup4 <- hclust(distancias_pt, method = "single", members = NULL)
agroup5 <- hclust(distancias_pt, method = "average", members = NULL)
agroup6 <- hclust(distancias_pt, method = "mcquitty", members = NULL)
agroup7 <- hclust(distancias_pt, method = "median", members = NULL)
agroup8 <- hclust(distancias_pt, method = "centroid", members = NULL)

# Los resultados de este análisis se visualizan mediante un dendograma, al mismo
# tiempo, se guardan los dendogramas en conjunto como un archivo png
# jpeg("03_Out/Plots/analisis_de_cluster_probabilidades_de_transicion.jpeg",
#      width = 5733, height = 5733, res = 600, units = "px")
layout ( matrix ( c( 1 : 9 ), 3, 3))
plot(agroup1)
plot(agroup2) 
plot(agroup3)
plot(agroup4)
plot(agroup5) 
plot(agroup6) 
plot(agroup7)
plot(agroup8)
layout(matrix (c ( 1 : 9 ), 3, 3))
# dev.off()

# Con la visualizacion de los diferentes dendogramas, se encuentra que hay 
# resultados que son consistentes y otros que difieren, por lo que se opta por
# definir los grupos de acuerdo a que tan recurrente fueron agrupados en los
# diferentes algoritmos de agrupamiento. Por lo que de esta manera los grupos 
# se definen como:
# ---------------- Grupo 1: Personas menores de 18 años
# ---------------- Grupo 2: Personas de 18 - 29 & 30 - 39
# ---------------- Grupo 3: Personas de 40 - 49 & 50 - 59
# ---------------- Grupo 4: Personas de 60 - 69 & Personas mayores de 70 años
