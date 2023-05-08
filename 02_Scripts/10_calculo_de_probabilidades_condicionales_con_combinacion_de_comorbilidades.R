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
library(ComplexHeatmap)
library(circlize)

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
                      name= "Valor", col = viridis(35), 
                      column_title = "Heatmap comorbilidades combinadas menores de 18 años",
                      na_col = "white",
                      row_names_side = "left",
                      cluster_rows = F, cluster_columns = F)
#
# C2
mat_combinaciones_c2 = probabilidades_combinadas_cat_2
heatmap_c2 <- Heatmap(mat_combinaciones_c2, 
                      name= "Valor", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas 18 - 39 años",
                      na_col = "white",
                      row_names_side = "left",
                      cluster_rows = F, cluster_columns = F)
#
# C3
mat_combinaciones_c3 = probabilidades_combinadas_cat_3
heatmap_c3 <- Heatmap(mat_combinaciones_c3, 
                      name= "Valor", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas 40 - 59 años",
                      na_col = "white",
                      row_names_side = "left",
                      cluster_rows = F, cluster_columns = F)
#
# C4
mat_combinaciones_c4 = probabilidades_combinadas_cat_4
heatmap_c4 <- Heatmap(mat_combinaciones_c4, 
                      name= "Valor", col = viridis(35),
                      column_title = "Heatmap comorbilidades combinadas mayores de 60 años",
                      na_col = "white",
                      row_names_side = "left",
                      cluster_rows = F, cluster_columns = F)
#
#
jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1.jpeg",
     width = 265, height = 265, res = 300, units = "mm")
heatmap_c1
dev.off()
jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2.jpeg",
     width = 265, height = 265, res = 300, units = "mm")
heatmap_c2
dev.off()
jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3.jpeg",
     width = 265, height = 265, res = 300, units = "mm")
heatmap_c3
dev.off()
jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4.jpeg",
     width = 265, height = 265, res = 300, units = "mm")
heatmap_c4
dev.off()
#
mat_combinaciones_c1
nm <- rownames(mat_combinaciones_c1)
col_fun <- colorRamp2(c(0, 0.0005, 0.001),  
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_c1_v2 <- Heatmap(mat_combinaciones_c1,
                         col = col_fun, name = "Valor",
                         column_title = "Combinacion comorbilidades",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, width, height, fill) {
                             grid.rect(x = x, y = y, width = width, height = height, 
                                       gp = gpar(col = "white", fill = NA))
                             if(i == j) {
                                 grid.text(nm[i], x = x, y = y, gp = gpar(fontsize = 7.5, fontface = "bold"))
                             } else if(i > j) {
                                 grid.circle(x = x, y = y, r = abs(mat_combinaciones_c1[i, j])/2 * min(unit.c(width, height)), 
                                             gp = gpar(fill = col_fun(mat_combinaciones_c1[i, j]), col = NA))
                             } else {
                                 grid.text(sprintf("%.5f", mat_combinaciones_c1[i, j]), x, y, gp = gpar(fontsize = 10))
                             }
                         })
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1_v2.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c1_v2
#dev.off()
