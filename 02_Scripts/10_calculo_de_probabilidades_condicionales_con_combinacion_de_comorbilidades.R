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
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipertension", 
        "Cardiovascular",
        "Obesidad",
        "Renal Crónica", 
        "Tabaquismo")
# C1
mat_combinaciones_c1 = probabilidades_combinadas_cat_1
heatmap_c1 <- Heatmap(mat_combinaciones_c1,
                      rect_gp = gpar(type = "none"), column_dend_side = "bottom",
                      cell_fun = function(j, i, x, y, w, h, fill) {
                              if(as.numeric(x) <= 1 - as.numeric(y) + 1e-6) {
                                      grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                                      }
                              },
                      cluster_columns = F, cluster_rows = F,
                      column_title = "Comorbilidades combinadas <18 años",
                      row_names_side = "left", col = viridis(35),
                      name = "Valor")

#
heatmap_c1_v3 <- Heatmap(mat_combinaciones_c1,
        col = viridis(35), name = "Valor",
        column_title = "Combinacion comorbilidades de <18 años",
        cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = F, show_column_names = F,
        rect_gp = gpar(type = "none"), 
        cell_fun = function(j, i, x, y, w, h, fill) {
            if(i == j) {
                grid.text(nm[i], x = x, y = y, 
                          gp = gpar(fontsize = 7.5, fontface = "bold"))
            } else if(i <= j) {
                grid.rect(x, y, w, h, gp = gpar(fill = fill, col = "white"))
            }
        })
#
#
# C2
mat_combinaciones_c2 = probabilidades_combinadas_cat_2
heatmap_c2 <- Heatmap(mat_combinaciones_c2,
                      rect_gp = gpar(type = "none"), column_dend_side = "bottom",
                      cell_fun = function(j, i, x, y, w, h, fill) {
                              if(as.numeric(x) <= 1 - as.numeric(y) + 1e-6) {
                                      grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                              }
                      },
                      cluster_columns = F, cluster_rows = F,
                      column_title = "Comorbilidades combinadas 18 - 39 años",
                      row_names_side = "left", col = viridis(35),
                      name = "Valor")
#
heatmap_c2_v3 <- Heatmap(mat_combinaciones_c2,
                         col = viridis(35), name = "Valor",
                         column_title = "Combinacion comorbilidades 18 - 39 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"), 
                         cell_fun = function(j, i, x, y, w, h, fill) {
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, 
                                                   gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i <= j) {
                                         grid.rect(x, y, w, h, gp = gpar(fill = fill, col = "white"))
                                 }
                         })
#
# C3
mat_combinaciones_c3 = probabilidades_combinadas_cat_3
heatmap_c3 <- Heatmap(mat_combinaciones_c3,
                      rect_gp = gpar(type = "none"), column_dend_side = "bottom",
                      cell_fun = function(j, i, x, y, w, h, fill) {
                              if(as.numeric(x) <= 1 - as.numeric(y) + 1e-6) {
                                      grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                              }
                      },
                      cluster_columns = F, cluster_rows = F,
                      column_title = "Comorbilidades combinadas 40 - 59 años",
                      row_names_side = "left", col = viridis(35),
                      name = "Valor")
#
#
heatmap_c3_v3 <- Heatmap(mat_combinaciones_c3,
                         col = viridis(35), name = "Valor",
                         column_title = "Combinacion comorbilidades 40 - 59 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"), 
                         cell_fun = function(j, i, x, y, w, h, fill) {
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, 
                                                   gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i <= j) {
                                         grid.rect(x, y, w, h, gp = gpar(fill = fill, col = "white"))
                                 }
                         })
#
# C4
mat_combinaciones_c4 = probabilidades_combinadas_cat_4
heatmap_c4 <- Heatmap(mat_combinaciones_c4,
                      rect_gp = gpar(type = "none"), column_dend_side = "bottom",
                      cell_fun = function(j, i, x, y, w, h, fill) {
                              if(as.numeric(x) <= 1 - as.numeric(y) + 1e-6) {
                                      grid.rect(x, y, w, h, gp = gpar(fill = fill, col = fill))
                              }
                      },
                      cluster_columns = F, cluster_rows = F,
                      column_title = "Comorbilidades combinadas >60 años",
                      row_names_side = "left", col = viridis(35),
                      name = "Valor")
#
heatmap_c4_v3 <- Heatmap(mat_combinaciones_c4,
                         col = viridis(35), name = "Valor",
                         column_title = "Combinacion comorbilidades >60 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"), 
                         cell_fun = function(j, i, x, y, w, h, fill) {
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, 
                                                   gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i <= j) {
                                         grid.rect(x, y, w, h, gp = gpar(fill = fill, col = "white"))
                                 }
                         })
#
#
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c1
#dev.off()
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
#
#
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1_v3.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c1_v3
#dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2_v3.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c2_v3
#dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3_v3.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c3_v3
#dev.off()
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4_v3.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c4_v3
#dev.off()
#
# Version 2
# Categoria 1: Menores de 18 años
mat_combinaciones_c1
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipertension", 
        "Cardiovascular",
        "Obesidad",
        "Renal Crónica", 
        "Tabaquismo")
col_fun <- colorRamp2(c(0, 0.0005, 0.001),  
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_c1_v2 <- Heatmap(mat_combinaciones_c1,
                         col = col_fun, name = "Valor",
                         column_title = "Combinacion comorbilidades <18 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, width, height, fill) {
                             grid.rect(x = x, y = y, width = width, height = height, 
                                       gp = gpar(col = "white", fill = NA))
                             if(i == j) {
                                 grid.text(nm[i], x = x, y = y, gp = gpar(fontsize = 7.5, fontface = "bold"))
                             } else if(i > j) {
                                 grid.circle(x = x, y = y, r = abs(mat_combinaciones_c1[i, j])/2 * min(unit.c(width, height))*1000, 
                                             gp = gpar(fill = col_fun(mat_combinaciones_c1[i, j]), col = NA))
                             } else {
                                 grid.text(sprintf("%.5f", mat_combinaciones_c1[i, j]), x, y, gp = gpar(fontsize = 10))
                             }
                         })
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c1_v2.jpeg",
#     width = 365, height = 365, res = 300, units = "mm")
heatmap_c1_v2
#dev.off()
#
# Categoria 2:18 - 39 años
mat_combinaciones_c2
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipertension", 
        "Cardiovascular",
        "Obesidad",
        "Renal Crónica", 
        "Tabaquismo")
col_fun <- colorRamp2(c(0, 0.003, 0.006),  
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_c2_v2 <- Heatmap(mat_combinaciones_c2,
                         col = col_fun, name = "Valor",
                         column_title = "Combinacion comorbilidades 18 - 39 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, width, height, fill) {
                                 grid.rect(x = x, y = y, width = width, height = height, 
                                           gp = gpar(col = "white", fill = NA))
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i > j) {
                                         grid.circle(x = x, y = y, r = abs(mat_combinaciones_c2[i, j])/2 * min(unit.c(width, height))*80, 
                                                     gp = gpar(fill = col_fun(mat_combinaciones_c2[i, j]), col = NA))
                                 } else {
                                         grid.text(sprintf("%.5f", mat_combinaciones_c2[i, j]), x, y, gp = gpar(fontsize = 10))
                                 }
                         })
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c2_v2.jpeg",
#     width = 365, height = 365, res = 300, units = "mm")
heatmap_c2_v2
#dev.off()
#
# Categoria 3: 40 - 59 años
mat_combinaciones_c3
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipertension", 
        "Cardiovascular",
        "Obesidad",
        "Renal Crónica", 
        "Tabaquismo")
col_fun <- colorRamp2(c(0, 0.0325, 0.065),  
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_c3_v2 <- Heatmap(mat_combinaciones_c3,
                         col = col_fun, name = "Valor",
                         column_title = "Combinacion comorbilidades 40 - 59 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, width, height, fill) {
                                 grid.rect(x = x, y = y, width = width, height = height, 
                                           gp = gpar(col = "white", fill = NA))
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i > j) {
                                         grid.circle(x = x, y = y, r = abs(mat_combinaciones_c3[i, j])/2 * min(unit.c(width, height))*15, 
                                                     gp = gpar(fill = col_fun(mat_combinaciones_c3[i, j]), col = NA))
                                 } else {
                                         grid.text(sprintf("%.5f", mat_combinaciones_c3[i, j]), x, y, gp = gpar(fontsize = 10))
                                 }
                         })
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c3_v2.jpeg",
#     width = 365, height = 365, res = 300, units = "mm")
heatmap_c3_v2
#dev.off()
#
# Categoria 4: >60 años
mat_combinaciones_c4
nm <- c("Diabetes",
        "EPOC",
        "Asma", 
        "Inmunsupr",
        "Hipertension", 
        "Cardiovascular",
        "Obesidad",
        "Renal Crónica", 
        "Tabaquismo")
col_fun <- colorRamp2(c(0, 0.11, 0.23),  
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_c4_v2 <- Heatmap(mat_combinaciones_c4,
                         col = col_fun, name = "Valor",
                         column_title = "Combinacion comorbilidades >60 años",
                         cluster_rows = FALSE, cluster_columns = FALSE,
                         show_row_names = F, show_column_names = F,
                         rect_gp = gpar(type = "none"),
                         cell_fun = function(j, i, x, y, width, height, fill) {
                                 grid.rect(x = x, y = y, width = width, height = height, 
                                           gp = gpar(col = "white", fill = NA))
                                 if(i == j) {
                                         grid.text(nm[i], x = x, y = y, gp = gpar(fontsize = 7.5, fontface = "bold"))
                                 } else if(i > j) {
                                         grid.circle(x = x, y = y, r = abs(mat_combinaciones_c4[i, j])/2 * min(unit.c(width, height))*4.5, 
                                                     gp = gpar(fill = col_fun(mat_combinaciones_c4[i, j]), col = NA))
                                 } else {
                                         grid.text(sprintf("%.5f", mat_combinaciones_c4[i, j]), x, y, gp = gpar(fontsize = 10))
                                 }
                         })
#jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_combinadas_c4_v2.jpeg",
#     width = 265, height = 265, res = 300, units = "mm")
heatmap_c4_v2
#dev.off()

# BIG MATRIX -----
## probabilidades combinadas
probabilidades_combinadas_cat_1
probabilidades_combinadas_cat_2
probabilidades_combinadas_cat_3
probabilidades_combinadas_cat_4
##hacer una nueva matriz
bm <- matrix(nrow = 4, ncol = 9)
rownames(bm) <- c("Menores 18", "18-39", "40-59", "Mayores 60")
colnames(bm) <- nm
##llenar los valores de la matriz
for (i in 1:ncol(t(matriz_comor)) ) {
  bm[,i] <- t(matriz_comor)[,i]
}
bm
#****
nr <- nrow(probabilidades_combinadas_cat_1)
##valores combinados CATEGORA 1
valores_sin_diagonal_1 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_1 <- c(valores_sin_diagonal_1,probabilidades_combinadas_cat_1[i, j])
    }
  }
}
valores_sin_diagonal_1
##valores combinados CATEGORA 2
valores_sin_diagonal_2 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_2 <- c(valores_sin_diagonal_2,probabilidades_combinadas_cat_2[i, j])
    }
  }
}
valores_sin_diagonal_2
##valores combinados CATEGORA 3
valores_sin_diagonal_3 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_3 <- c(valores_sin_diagonal_3,probabilidades_combinadas_cat_3[i, j])
    }
  }
}
valores_sin_diagonal_3
##valores combinados CATEGORA 4
valores_sin_diagonal_4 <- c()
for (i in 1:nr) {
  for (j in 1:nr) {
    if (i != j && i < j) {  # Asegura que no estamos en la diagonal y que no duplicamos
      valores_sin_diagonal_4 <- c(valores_sin_diagonal_4,probabilidades_combinadas_cat_4[i, j])
    }
  }
}
valores_sin_diagonal_4
##llenar los valores en la matriz
combin <- rbind(valores_sin_diagonal_1,
                valores_sin_diagonal_2,
                valores_sin_diagonal_3,
                valores_sin_diagonal_4)
bm <- cbind(bm, combin)
bm
colnames(bm) <- c("Diabetes","EPOC","Asma","Inmunsupr","Hipertension",
                  "Cardiovascular","Obesidad","Renal Crónica","Tabaquismo",
                  "Diabetes X EPOC", "Diabetes X Asma", "Diabetes X Inmunsupr",
                  "Diabetes X Hipertension", "Diabetes X Cadiovascular", 
                  "Diabetes X Obesidad", "Diabetes X Renal Cronica", 
                  "Diabetes X Tabaquismo", "EPOC X Asma", "EPOC X Inmunsupr",
                  "EPOC X Hipertension", "EPOC X Cardiovascular", 
                  "EPOC X Obesidad", "EPOC X Renal Cronica", 
                  "EPOC X Tabaquismo", "Asma X Inmunsupr", 
                  "Asma X Hipertension", "Asma X Cardiovascular", 
                  "Asma X Obesidad", "Asma X Renal Cronica", 
                  "Asma X Tabaquismo","Inmunsupr X Hipertension", 
                  "Inmunsupr X Cardiovascular", "Inmunsupr X Obesidad", 
                  "Inmunsupr X Renal Cronica", "Inmunsupr X Tabaquismo",
                  "Hipertension X Cardiovascular", "Hipertension X Obesidad", 
                  "Hipertension X Renal Cronica", "Hipertension X Tabaquismo",
                  "Cardiovascular X Obesidad", "Cardiovascular X Renal Cronica",
                  "Cardiovascular X Tabaquismo","Obesidad X Renal Cronica",
                  "Obesidad X Tabaquismo","Renal Cronica X Tabaquismo")
bm
mat_combinaciones_bm = bm
bm_heatmap <- Heatmap(mat_combinaciones_bm,
                      name = "p",col = viridis(45),
                      column_title = "Heatmap Big Matrix",
                      rect_gp = gpar(col = "white", lwd = 2),
                      column_title_gp = gpar(fontsize = 12, fontface = "bold"),
                      cluster_rows = FALSE, cluster_columns = FALSE,
                      column_names_side = "top", column_dend_side = "bottom",
                      column_names_gp = gpar(fontsize = 6),
                      row_names_side = "left"
                      )
bm_heatmap
#jpeg("03_Out/Plots/bm_heatmap.jpeg",
#     width = 465, height = 365, res = 300, units = "mm")
bm_heatmap
#dev.off()