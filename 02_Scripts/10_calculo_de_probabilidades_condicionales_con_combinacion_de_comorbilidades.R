# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)
library(ggmatplot)


# Se carga la base de datos
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad.RData")
load("03_Out/OutData/conteo_comorbilidades.RData")
load("03_Out/OutData/conteo_SIN_comorbilidades.RData")
load("03_Out/OutData/matriz_p_comorbilidades.RData")
load("03_Out/OutData/matriz_2_p_comorbilidades.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad_cat_1.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad_cat_2.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad_cat_3.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad_cat_4.RData")


# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# P ( CAT_i & COM_j & COM_k )
casos_pos_re_comorbilidad

conteo_comor_combinated_1 <- comorbilidades_combinadas_conteo(casos_pos_re_comorbilidad_cat_1)
conteo_comor_combinated_2 <- comorbilidades_combinadas_conteo(casos_pos_re_comorbilidad_cat_2)
conteo_comor_combinated_3 <- comorbilidades_combinadas_conteo(casos_pos_re_comorbilidad_cat_3)
conteo_comor_combinated_4 <- comorbilidades_combinadas_conteo(casos_pos_re_comorbilidad_cat_4)
conteo_comor_combinated_1
conteo_comor_combinated_2
conteo_comor_combinated_3
conteo_comor_combinated_4

# Se dividen los conteos de la combinacion de comorbilidades entre la poblacion
# de la categoria
#
# P(com&com|cat)
probabilidades_combinadas_cat_1 <- probabilidades_combinaciones(casos_pos_re_comorbilidad_cat_1,
                                                          conteo_comor_combinated_1)
probabilidades_combinadas_cat_2 <- probabilidades_combinaciones(casos_pos_re_comorbilidad_cat_2,
                                                                conteo_comor_combinated_2)
probabilidades_combinadas_cat_3 <- probabilidades_combinaciones(casos_pos_re_comorbilidad_cat_3,
                                                                conteo_comor_combinated_3)
probabilidades_combinadas_cat_4 <- probabilidades_combinaciones(casos_pos_re_comorbilidad_cat_4,
                                                                conteo_comor_combinated_4)
probabilidades_combinadas_cat_1
probabilidades_combinadas_cat_2
probabilidades_combinadas_cat_3
probabilidades_combinadas_cat_4

# Heatmap
# C1
mat_combinaciones_c1 = probabilidades_combinadas_cat_1
heatmap_c1 <- Heatmap(mat_combinaciones_c1, 
                      name= "p(com|com)", col = viridis(35), 
                      column_title = "Heatmap comorbilidades combinadas categoria 1")
#
# C2
mat_combinaciones_c2 = probabilidades_combinadas_cat_2
heatmap_c2 <- Heatmap(mat_combinaciones_c2, 
                      name= "p(com|com)", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas categoria 2")
#
# C3
mat_combinaciones_c3 = probabilidades_combinadas_cat_3
heatmap_c3 <- Heatmap(mat_combinaciones_c3, 
                      name= "p(com|com)", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas categoria 3")
#
# C4
mat_combinaciones_c4 = probabilidades_combinadas_cat_4
heatmap_c4 <- Heatmap(mat_combinaciones_c4, 
                      name= "p(com|com)", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas categoria 4")
#
#
jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1.jpeg",
     width = 265, height = 265, res = 300, units = "mm")
heatmap_c1
dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c2
#dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c3
#dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c4
#dev.off()

