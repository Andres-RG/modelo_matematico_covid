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
# Conteos de comorbilidades ====================================================
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

# Cálculo de probabiidades =====================================================
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
#
#
## vector de nombres
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipert", 
        "Cardiovas",
        "Obesidad",
        "Renal Cr", 
        "Tabaquismo")
#
#

# Graficas =====================================================================
## VERSION 1 -------------------------------------------------------------------
### CATEGORIA 1 --------------------------------------
#### Ajuste de la matriz
mat_combinaciones_c1 = probabilidades_combinadas_cat_1
colnames(mat_combinaciones_c1) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
rownames(mat_combinaciones_c1) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
mat_combinaciones_c1
#### Grafica
heatmap_c1 <- Heatmap(mat_combinaciones_c1       ,
                      #
                      cell_fun = function(j, i, x, y, w, h, fill) {
                        if(i >= j) {
                          grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                        }
                      },
                      #
                      rect_gp = gpar(type = "none"),
                      name = "Valor"             ,
                      col = viridis(100)         ,
                      # columnas
                      cluster_columns = F        ,
                      column_names_rot = 0       ,
                      column_names_side = "top",
                      column_title = "Comorbilidades",
                      column_title_gp = gpar(fontsize = 11, fontface = "bold",
                                             fill = "gray", border = "gray"),
                      column_names_centered = T  ,
                      column_names_gp = gpar(fontsize = 11),
                      # renglones
                      cluster_rows = F           ,
                      row_names_side = "left"    ,
                      row_title = "Comorbilidades",
                      row_title_gp = gpar(fontsize = 11, fontface = "bold",
                                          fill = "gray", border = "gray"),
                      row_names_centered = T,
                      row_names_gp = gpar(fontsize = 11)
                      )
heatmap_c1 <- draw(heatmap_c1,
                   column_title = "Menores 18 años",
                   column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c1
# dev.off()

### CATEGORIA 2 --------------------------------------
#### Ajuste de la matriz
mat_combinaciones_c2 = probabilidades_combinadas_cat_2
colnames(mat_combinaciones_c2) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
rownames(mat_combinaciones_c2) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
mat_combinaciones_c2
#### Grafica
heatmap_c2 <- Heatmap(mat_combinaciones_c2,
                      #
                      cell_fun = function(j, i, x, y, w, h, fill) {
                        if(i >= j) {
                          grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                        }
                      },
                      #
                      rect_gp = gpar(type = "none"),
                      name = "Valor"             ,
                      col = viridis(100)         ,
                      # columnas
                      cluster_columns = F        ,
                      column_names_rot = 0       ,
                      column_names_side = "top",
                      column_title = "Comorbilidades",
                      column_title_gp = gpar(fontsize = 11, fontface = "bold",
                                             fill = "gray", border = "gray"),
                      column_names_centered = T  ,
                      column_names_gp = gpar(fontsize = 11),
                      # renglones
                      cluster_rows = F           ,
                      row_names_side = "left"    ,
                      row_title = "Comorbilidades",
                      row_title_gp = gpar(fontsize = 11, fontface = "bold",
                                          fill = "gray", border = "gray"),
                      row_names_centered = T,
                      row_names_gp = gpar(fontsize = 11)
)
heatmap_c2 <- draw(heatmap_c2,
                   column_title = "18 a 39 años",
                   column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c2
# dev.off()

### CATEGORIA 3 --------------------------------------
#### Ajuste de la matriz
mat_combinaciones_c3 = probabilidades_combinadas_cat_3
colnames(mat_combinaciones_c3) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
rownames(mat_combinaciones_c3) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
mat_combinaciones_c3
#### Grafica
heatmap_c3 <- Heatmap(mat_combinaciones_c3,
                      #
                      cell_fun = function(j, i, x, y, w, h, fill) {
                        if(i >= j) {
                          grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                        }
                      },
                      #
                      rect_gp = gpar(type = "none"),
                      name = "Valor"             ,
                      col = viridis(100)         ,
                      # columnas
                      cluster_columns = F        ,
                      column_names_rot = 0       ,
                      column_names_side = "top",
                      column_title = "Comorbilidades",
                      column_title_gp = gpar(fontsize = 11, fontface = "bold",
                                             fill = "gray", border = "gray"),
                      column_names_centered = T  ,
                      column_names_gp = gpar(fontsize = 11),
                      # renglones
                      cluster_rows = F           ,
                      row_names_side = "left"    ,
                      row_title = "Comorbilidades",
                      row_title_gp = gpar(fontsize = 11, fontface = "bold",
                                          fill = "gray", border = "gray"),
                      row_names_centered = T,
                      row_names_gp = gpar(fontsize = 11)
)
heatmap_c3 <- draw(heatmap_c3,
                   column_title = "40 a 59 años",
                   column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c3
# dev.off()

### CATEGORIA 4 --------------------------------------
#### Ajuste de la matriz
mat_combinaciones_c4 = probabilidades_combinadas_cat_4
colnames(mat_combinaciones_c4) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
rownames(mat_combinaciones_c4) <- c("D","E", "A", "I", "H", "CV", "O", "RC", "T")
mat_combinaciones_c4
#### Grafica
heatmap_c4 <- Heatmap(mat_combinaciones_c4,
                      #
                      cell_fun = function(j, i, x, y, w, h, fill) {
                        if(i >= j) {
                          grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                        }
                      },
                      #
                      rect_gp = gpar(type = "none"),
                      name = "Valor"             ,
                      col = viridis(100)         ,
                      # columnas
                      cluster_columns = F        ,
                      column_names_rot = 0       ,
                      column_names_side = "top",
                      column_title = "Comorbilidades",
                      column_title_gp = gpar(fontsize = 11, fontface = "bold",
                                             fill = "gray", border = "gray"),
                      column_names_centered = T  ,
                      column_names_gp = gpar(fontsize = 11),
                      # renglones
                      cluster_rows = F           ,
                      row_names_side = "left"    ,
                      row_title = "Comorbilidades",
                      row_title_gp = gpar(fontsize = 11, fontface = "bold",
                                          fill = "gray", border = "gray"),
                      row_names_centered = T,
                      row_names_gp = gpar(fontsize = 11)
)
heatmap_c4 <- draw(heatmap_c4,
                   column_title = "Mayores de 60 años",
                   column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c4
# dev.off()

## VERSION 2 -------------------------------------------------------------------
### CATEGORIA 1 --------------------------------------
# Generar cinco colores
col_fun_1 <- colorRamp2(c(0, 0.00024, 0.00048, 0.00072, 0.00096),
                        c("white",
                          "lightskyblue",
                          "deepskyblue",
                          "dodgerblue4",
                          "blue4"))
mat_combinaciones_c1
#### Grafica
heatmap_c1_v2 <- Heatmap(mat_combinaciones_c1,
                         col = col_fun_1,
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         #
                         cell_fun = function(j, i, x, y, width, height, fill) {
                           grid.rect(x = x,
                                     y = y,
                                     width = width,
                                     height = height, 
                                     gp = gpar(col = "white",
                                               fill = NA,
                                               alpha = 0.5))
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i > j) {
                             grid.circle(x = x,
                                         y = y,
                                         r = abs(mat_combinaciones_c1[i, j])/2 * min(unit.c(width, height))*1000, 
                                         gp = gpar(fill = col_fun_1(mat_combinaciones_c1[i, j]), col = NA))
                           } else {
                             grid.text(sprintf("%.5f", mat_combinaciones_c1[i, j]), x, y, gp = gpar(fontsize =13))
                           }
                         }
)
heatmap_c1_v2 <- draw(heatmap_c1_v2,
                      column_title = "Menores 18 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1_v2.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c1_v2
# dev.off()

### CATEGORIA 2 --------------------------------------
# Generar cinco colores
col_fun_2 <- colorRamp2(c(0, 0.003515, 0.00703, 0.010545, 0.01406),
                        c("white",
                          "lightskyblue",
                          "deepskyblue",
                          "dodgerblue4",
                          "blue4"))
mat_combinaciones_c2
#### Grafica
heatmap_c2_v2 <- Heatmap(mat_combinaciones_c2,
                         col = col_fun_2,
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         #
                         cell_fun = function(j, i, x, y, width, height, fill) {
                           grid.rect(x = x,
                                     y = y,
                                     width = width,
                                     height = height, 
                                     gp = gpar(col = "white",
                                               fill = NA,
                                               alpha = 0.5))
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i > j) {
                             grid.circle(x = x,
                                         y = y,
                                         r = abs(mat_combinaciones_c2[i, j])/2 * min(unit.c(width, height))*70, 
                                         gp = gpar(fill = col_fun_2(mat_combinaciones_c2[i, j]), col = NA))
                           } else {
                             grid.text(sprintf("%.5f", mat_combinaciones_c2[i, j]), x, y, gp = gpar(fontsize =13))
                           }
                         }
)
heatmap_c2_v2 <- draw(heatmap_c2_v2,
                      column_title = "18 a 39 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2_v2.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c2_v2
# dev.off()

### CATEGORIA 3 --------------------------------------
# Generar cinco colores
col_fun_3 <- colorRamp2(c(0, 0.016775, 0.03355, 0.050325, 0.0671),
                        c("white",
                          "lightskyblue",
                          "deepskyblue",
                          "dodgerblue4",
                          "blue4"))
mat_combinaciones_c3
#### Grafica
heatmap_c3_v2 <- Heatmap(mat_combinaciones_c3,
                         col = col_fun_3,
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         #
                         cell_fun = function(j, i, x, y, width, height, fill) {
                           grid.rect(x = x,
                                     y = y,
                                     width = width,
                                     height = height, 
                                     gp = gpar(col = "white",
                                               fill = NA,
                                               alpha = 0.5))
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i > j) {
                             grid.circle(x = x,
                                         y = y,
                                         r = abs(mat_combinaciones_c3[i, j])/2 * min(unit.c(width, height))*15, 
                                         gp = gpar(fill = col_fun_3(mat_combinaciones_c3[i, j]), col = NA))
                           } else {
                             grid.text(sprintf("%.5f", mat_combinaciones_c3[i, j]), x, y, gp = gpar(fontsize =13))
                           }
                         }
)
heatmap_c3_v2 <- draw(heatmap_c3_v2,
                      column_title = "40 a 59 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3_v2.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c3_v2
# dev.off()

### CATEGORIA 4 --------------------------------------
# Generar cinco colores
col_fun_4 <- colorRamp2(c(0, 0.0553825, 0.110765, 0.1661475, 0.22153),
                        c("white",
                          "lightskyblue",
                          "deepskyblue",
                          "dodgerblue4",
                          "blue4"))
mat_combinaciones_c4
#### Grafica
heatmap_c4_v2 <- Heatmap(mat_combinaciones_c4,
                         col = col_fun_4,
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         #
                         cell_fun = function(j, i, x, y, width, height, fill) {
                           grid.rect(x = x,
                                     y = y,
                                     width = width,
                                     height = height, 
                                     gp = gpar(col = "white",
                                               fill = NA,
                                               alpha = 0.5))
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i > j) {
                             grid.circle(x = x,
                                         y = y,
                                         r = abs(mat_combinaciones_c4[i, j])/2 * min(unit.c(width, height))*4.5, 
                                         gp = gpar(fill = col_fun_4(mat_combinaciones_c4[i, j]), col = NA))
                           } else {
                             grid.text(sprintf("%.5f", mat_combinaciones_c4[i, j]), x, y, gp = gpar(fontsize =13))
                           }
                         }
)
heatmap_c4_v2 <- draw(heatmap_c4_v2,
                      column_title = "Mayores de 60 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4_v2.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c4_v2
# dev.off()

## VERSION 3 -------------------------------------------------------------------
### CATEGORIA 1 --------------------------------------
heatmap_c1_v3 <- Heatmap(mat_combinaciones_c1,
                         col = viridis(100),
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, w, h, fill) {
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                             } else if(i <= j) {
                               grid.rect(x, y, w, h, gp = gpar(fill = fill,
                                                               col = "white"))
                             }
                           }
)
heatmap_c1_v3 <- draw(heatmap_c1_v3,
                      column_title = "Menores 18 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1_v3.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c1_v3
# dev.off()

### CATEGORIA 2 --------------------------------------
heatmap_c2_v3 <- Heatmap(mat_combinaciones_c2,
                         col = viridis(100),
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, w, h, fill) {
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i <= j) {
                             grid.rect(x, y, w, h, gp = gpar(fill = fill,
                                                             col = "white"))
                           }
                         }
)
heatmap_c2_v3 <- draw(heatmap_c2_v3,
                      column_title = "18 a 39 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2_v3.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c2_v3
# dev.off()

### CATEGORIA 3 --------------------------------------
heatmap_c3_v3 <- Heatmap(mat_combinaciones_c3,
                         col = viridis(100),
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, w, h, fill) {
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i <= j) {
                             grid.rect(x, y, w, h, gp = gpar(fill = fill,
                                                             col = "white"))
                           }
                         }
)
heatmap_c3_v3 <- draw(heatmap_c3_v3,
                      column_title = "40 a 59 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))
# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3_v3.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c3_v3
# dev.off()

### CATEGORIA 4 --------------------------------------
heatmap_c4_v3 <- Heatmap(mat_combinaciones_c4,
                         col = viridis(100),
                         name = "Valor",
                         cluster_rows = FALSE,
                         cluster_columns = FALSE,
                         show_row_names = F,
                         show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, w, h, fill) {
                           if(i == j) {
                             grid.text(nm[i],
                                       x = x,
                                       y = y,
                                       gp = gpar(fontsize = 10,
                                                 fontface = "bold"))
                           } else if(i <= j) {
                             grid.rect(x, y, w, h, gp = gpar(fill = fill,
                                                             col = "white"))
                           }
                         }
)
heatmap_c4_v3 <- draw(heatmap_c4_v3,
                      column_title = "Mayores de 60 años",
                      column_title_gp = grid::gpar(fontsize=15, fontface = "bold"))

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4_v3.jpeg",
#      width = 5733, height = 5733, res = 800, units = "px")
# heatmap_c4_v3
# dev.off()

# BIG MATRIX ===================================================================
## probabilidades combinadas --------------------------------------------
probabilidades_combinadas_cat_1
probabilidades_combinadas_cat_2
probabilidades_combinadas_cat_3
probabilidades_combinadas_cat_4
## hacer una nueva matriz -----------------------------------------------
bm <- matrix(nrow = 4, ncol = 9)
rownames(bm) <- c("Menores 18", "18-39", "40-59", "Mayores 60")
colnames(bm) <- nm
## llenar los valores de la matriz --------------------------------------
for (i in 1:ncol(t(matriz_comor)) ) {
  bm[,i] <- t(matriz_comor)[,i]
}
bm
nr <- nrow(probabilidades_combinadas_cat_1)
## valores combinados CATEGORA 1 ----------------------------------------
valores_sin_diagonal_1 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_1 <- c(valores_sin_diagonal_1,probabilidades_combinadas_cat_1[i, j])
    }
  }
}
valores_sin_diagonal_1
## valores combinados CATEGORA 2 ----------------------------------------
valores_sin_diagonal_2 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_2 <- c(valores_sin_diagonal_2,probabilidades_combinadas_cat_2[i, j])
    }
  }
}
valores_sin_diagonal_2
## valores combinados CATEGORA 3 ----------------------------------------
valores_sin_diagonal_3 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_3 <- c(valores_sin_diagonal_3,probabilidades_combinadas_cat_3[i, j])
    }
  }
}
valores_sin_diagonal_3
## valores combinados CATEGORA 4 ----------------------------------------
valores_sin_diagonal_4 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_4 <- c(valores_sin_diagonal_4,probabilidades_combinadas_cat_4[i, j])
    }
  }
}
valores_sin_diagonal_4
## llenar los valores en la matriz --------------------------------------
combin <- rbind(valores_sin_diagonal_1,
                valores_sin_diagonal_2,
                valores_sin_diagonal_3,
                valores_sin_diagonal_4)
bm <- cbind(bm, combin)
bm
colnames(bm) <- c("D","E","A","I","H","CV","O","RC","T",
                  #
                  "D X E", "D X A", "D X I","D X H", "D X CV","D X O", "D X RC", 
                  "D X T",
                  #
                  "E X A", "E X I","E X H", "E X CV","E X O", "E X RC","E X T",
                  #
                  "A X I","A X H", "A X CV","A X O", "A X RC","A X T",
                  #
                  "I X H","I X CV", "I X O","I X RC","I X T",
                  #
                  "H X CV", "H X O","H X RC", "H X T",
                  #
                  "CV X O", "CV X RC","CV X T",
                  #
                  "O X RC","O X T",
                  #
                  "RC X T")
bm
mat_combinaciones_bm = bm
#
set.seed(123)
bm_heatmap <- Heatmap(mat_combinaciones_bm,
                      name= "valor",
                      col = viridis(100),
                      row_names_side = "left",
                      cluster_rows = F,
                      cluster_columns = F,
                      row_title = "Grupos etarios",
                      column_title = "Comorbilidades",
                      column_names_side = "top",
                      column_dend_side = "bottom",
                      column_names_rot = 90,
                      column_title_gp = gpar(fill = "gray",
                                             border = "gray",
                                             fontsize = 9,
                                             fontface = "bold"),
                      row_title_gp = gpar(fill = "gray",
                                          border = "gray",
                                          fontsize = 9,
                                          fontface = "bold"),
                      rect_gp = gpar(col = "white",
                                     lwd = 0.5),
                      row_names_gp = gpar(fontsize = 10, 
                                          fontface = "bold"),
                      column_names_gp = gpar(fontsize = 10,
                                             fontface = "bold")
)
bm_heatmap


# jpeg("03_Out/Plots/bm_heatmap.jpeg",
#      width = 5733, height = 4300, res = 800, units = "px")
# bm_heatmap
# dev.off()