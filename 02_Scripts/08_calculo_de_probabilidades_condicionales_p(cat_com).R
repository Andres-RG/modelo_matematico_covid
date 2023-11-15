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
library(devtools)
library(ComplexHeatmap)
library(circlize)

# Se carga la base de datos
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_re_comorbilidad.RData")
load("03_Out/OutData/conteo_comorbilidades.RData")
load("03_Out/OutData/conteo_SIN_comorbilidades.RData")
load("03_Out/OutData/matriz_p_comorbilidades.RData")

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

#¿Cuál es la probabilidad que una persona de cierta edad tenga una comorbilidad, 
# dos, tres , cuatro o combinaciones de éstas ?
#  probabilidad de que dado que tienen una comorbilidad, a qué categoría de edad
#  es más probable que  pertenezcan
#  p( cat_j | com_i )

# 1 Ajustar los datos ==========================================================
# 1.1 Seleccionar solo las columnas de interes con comorbilidades
casos_pos_re_comorbilidad <- select(casos_positivos_re, 
                                    c(FECHA_SINTOMAS, EDAD, INTUBADO, NEUMONIA,
                                      DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, 
                                      CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, 
                                      TABAQUISMO, rango_de_edad))
# 1.1 Se ordenan por rango de edad, de menor a mayor
casos_pos_re_comorbilidad <- arrange(casos_pos_re_comorbilidad, rango_de_edad)
# 1.2 Se agrega una columna a toda la base de datos, que contiene un 1, esto 
#     para sumar posteriormente y que resulte mas facil.
casos_pos_re_comorbilidad <- mutate(casos_pos_re_comorbilidad, ind = 1)
# 1.3 Se guarda como archivo
# save(casos_pos_re_comorbilidad, file = "03_Out/OutData/casos_positivos_re_comorbilidad.RData")
# 1.4 Se separa la base de datos de acuerdo al rango de edad
# 1.4.1 cat 1. Menores de 18 años
casos_pos_re_comorbilidad_cat_1 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-")
# save(casos_pos_re_comorbilidad_cat_1, file = "03_Out/OutData/casos_positivos_re_comorbilidad_cat_1.RData")
# 1.4.2 cat 2. 18 a 39 años
casos_pos_re_comorbilidad_cat_2 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-29" | rango_de_edad == "30-39")
# save(casos_pos_re_comorbilidad_cat_2, file = "03_Out/OutData/casos_positivos_re_comorbilidad_cat_2.RData")
# 1.4.3 cat 3. 40 a 59 años
casos_pos_re_comorbilidad_cat_3 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "40-49" | rango_de_edad == "50-59")
# save(casos_pos_re_comorbilidad_cat_3, file = "03_Out/OutData/casos_positivos_re_comorbilidad_cat_3.RData")
# 1.4.4 cat 4. 60 años en adelante
casos_pos_re_comorbilidad_cat_4 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "60-69" | rango_de_edad == "70+")
# save(casos_pos_re_comorbilidad_cat_4, file = "03_Out/OutData/casos_positivos_re_comorbilidad_cat_4.RData")

# 2 Determinacion de probabilidad ==============================================
# p( cat_i | com_j ) = P ( COM_j & CAT_i )
#                      --- --- --- --- ---
#                         P ( COM_j )
# 2.1 Obtencion de P ( COM_j & CAT_i )
#.    # casos de comorbilidad de la cat_i
#.    --- --- --- --- --- --- --- --- ---
#.            # casos de la cat_i
# 2.1.1 Conteo de casos con comorbilidades ----
# 2.1.1.1 Se obtienen los conteos por categoria de cuantos pacientes tienen alguna
#         comorbilidad, se hace por cada una, y para cada categoria.
#         * Se hace uso de una funcion.
com_cat_1 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_1)
com_cat_2 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_2)
com_cat_3 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_3)
com_cat_4 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_4)
# 2.1.1.2 Se compila la informacion obtenida en una base de datos
comorbilidades_conteos <- data.frame(com_cat_1,com_cat_2,com_cat_3,com_cat_4)
rownames(comorbilidades_conteos) <- c("diabetes"         ,
                                      "epoc"             ,
                                      "asma"             ,
                                      "inmunsupr"        ,
                                      "hipertension"     ,
                                      "cardiovascular"   ,
                                      "obesidad"         ,
                                      "renal_cronica"    ,
                                      "tabaquismo"       )
colnames(comorbilidades_conteos) <- c("categoria 1", "categoria 2", 
                                      "categoria 3", "categoria 4")
comorbilidades_conteos
# 2.1.1.2 Se guaradan los conteos de las personas con comorbilidades
# save(comorbilidades_conteos, file = "03_Out/OutData/conteo_comorbilidades.RData")
###################################################################################
# 2.1.1.3 Se hace el mismo procedimiento para pacientes SIN COMORBILIDADES*
#         Se realizo una funcion similar a la anterior, solo que para este caso
#         cuenta todos los casos que no tienen alguna comorbilidad
sin_com_cat_1 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_1)
sin_com_cat_2 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_2)
sin_com_cat_3 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_3)
sin_com_cat_4 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_4)
# 2.1.1.4 Se compila la información obtenida en una base de datos
sin_comorbilidades_conteos <- data.frame(sin_com_cat_1,sin_com_cat_2,
                                         sin_com_cat_3,sin_com_cat_4)
rownames(sin_comorbilidades_conteos) <- c("NO diabetes"         ,
                                          "NO epoc"             ,
                                          "NO asma"             ,
                                          "NO inmunsupr"        ,
                                          "NO hipertension"     ,
                                          "NO cardiovascular"   ,
                                          "NO obesidad"         ,
                                          "NO renal_cronica"    ,
                                          "NO tabaquismo"       )
colnames(sin_comorbilidades_conteos) <- c("categoria 1", "categoria 2",
                                          "categoria 3", "categoria 4")
sin_comorbilidades_conteos
# 2.1.1.5 Se guarda la base de datos
# save(comorbilidades_conteos, file = "03_Out/OutData/conteo_SIN_comorbilidades.RData")
###################################################################################

# 2.1.2 Conteo de individuos por categoria ----
# 2.1.2.1      CATEGORIA 1: MENORES DE 18 AÑOS
c1 <- nrow(casos_pos_re_comorbilidad_cat_1)
# 2.1.2.2      CATEGIRIA 2: 18 - 39 AÑOS
c2 <- nrow(casos_pos_re_comorbilidad_cat_2)
# 2.1.2.3      CATEGORIA 3: 40 - 59 AÑOS
c3 <- nrow(casos_pos_re_comorbilidad_cat_3)
# 2.1.2.4      CATEOGIRA 4: 60 AÑOS EN ADELANTE
c4 <- nrow(casos_pos_re_comorbilidad_cat_4)

# 2.1.3 Determinación de la probabilidad P ( COM_j & CAT_i )----
#       Se dividen los casos de comorbilidad, por comorbilidad, de cada 
#       categoria entre el total de casos de esa categoria
# 2.1.3.1 ---- CATEGORIA 1 
# 2.1.3.1.1 p( diabetes | c1 )
p_diab_c1 <- comorbilidades_conteos[1,1]/c1
# 2.1.3.1.2 p( epoc | c1 )
p_epoc_c1 <- comorbilidades_conteos[2,1]/c1
# 2.1.3.1.3 p( asma | c1 )
p_asma_c1 <- comorbilidades_conteos[3,1]/c1
# 2.1.3.1.4 p( inmunsupr | c1 )
p_inmunsupr_c1 <- comorbilidades_conteos[4,1]/c1
# 2.1.3.1.5 p( hipertension | c1 )
p_hipertension_c1 <- comorbilidades_conteos[5,1]/c1
# 2.1.3.1.6 p( cardiovascular | c1 )
p_cardiovascular_c1 <- comorbilidades_conteos[6,1]/c1
# 2.1.3.1.7 p( obesidad | c1 )
p_obesidad_c1 <- comorbilidades_conteos[7,1]/c1
# 2.1.3.1.8 p( renal_cronica | c1 )
p_renal_cronica_c1 <- comorbilidades_conteos[8,1]/c1
# 2.1.3.1.9 p( tabaquismo | c1 )
p_tabaquismo_c1 <- comorbilidades_conteos[9,1]/c1
# 2.1.3.1.10 Se guardan la p´s en un vector
p_comorb_c1 <- c(p_diab_c1,p_epoc_c1,p_asma_c1,p_inmunsupr_c1,
                 p_hipertension_c1,p_cardiovascular_c1,p_obesidad_c1,
                 p_renal_cronica_c1,p_tabaquismo_c1)

# 2.1.3.2 ---- CATEGORIA 2
# 2.1.3.2.1 p( diabetes | c2 )
p_diab_c2 <- comorbilidades_conteos[1,2]/c2
# 2.1.3.2.2 p( epoc | c2 )
p_epoc_c2 <- comorbilidades_conteos[2,2]/c2
# 2.1.3.2.3 p( asma | c2 )
p_asma_c2 <- comorbilidades_conteos[3,2]/c2
# 2.1.3.2.4 p( inmunsupr | c2 )
p_inmunsupr_c2 <- comorbilidades_conteos[4,2]/c2
# 2.1.3.2.5 p( hipertension | c2 )
p_hipertension_c2 <- comorbilidades_conteos[5,2]/c2
# 2.1.3.2.6 p( cardiovascular | c2 )
p_cardiovascular_c2 <- comorbilidades_conteos[6,2]/c2
# 2.1.3.2.7 p( obesidad | c2 )
p_obesidad_c2 <- comorbilidades_conteos[7,2]/c2
# 2.1.3.2.8 p( renal_cronica | c2 )
p_renal_cronica_c2 <- comorbilidades_conteos[8,2]/c2
# 2.1.3.2.9 p( tabaquismo | c2 )
p_tabaquismo_c2 <- comorbilidades_conteos[9,2]/c2
# 2.1.3.2.10 Se guardan las p´s en un vector 
p_comorb_c2 <- c(p_diab_c2,p_epoc_c2,p_asma_c2,p_inmunsupr_c2,
                 p_hipertension_c2,p_cardiovascular_c2,p_obesidad_c2,
                 p_renal_cronica_c2,p_tabaquismo_c2)


# 2.1.3.3 ---- CATEGORIA 3
# 2.1.3.3.1 p( diabetes | c3 )
p_diab_c3 <- comorbilidades_conteos[1,3]/c3
# 2.1.3.3.2 p( epoc | c3 )
p_epoc_c3 <- comorbilidades_conteos[2,3]/c3
# 2.1.3.3.3 p( asma | c3 )
p_asma_c3 <- comorbilidades_conteos[3,3]/c3
# 2.1.3.3.4 p( inmunsupr | c3 )
p_inmunsupr_c3 <- comorbilidades_conteos[4,3]/c3
# 2.1.3.3.5 p( hipertension | c3 )
p_hipertension_c3 <- comorbilidades_conteos[5,3]/c3
# 2.1.3.3.6 p( cardiovascular | c3 )
p_cardiovascular_c3 <- comorbilidades_conteos[6,3]/c3
# 2.1.3.3.7 p( obesidad | c3 )
p_obesidad_c3 <- comorbilidades_conteos[7,3]/c3
# 2.1.3.3.8 p( renal_cronica | c3 )
p_renal_cronica_c3 <- comorbilidades_conteos[8,3]/c3
# 2.1.3.3.9 p( tabaquismo | c3 )
p_tabaquismo_c3 <- comorbilidades_conteos[9,3]/c3
# 2.1.3.2.10 Se guardan las p´s en un vector 
p_comorb_c3 <- c(p_diab_c3,p_epoc_c3,p_asma_c3,p_inmunsupr_c3,
                 p_hipertension_c3,p_cardiovascular_c3,p_obesidad_c3,
                 p_renal_cronica_c3,p_tabaquismo_c3)


# 2.1.3.4 ---- CATEGORIA 4
# 2.1.3.4.1 p( diabetes | c4 )
p_diab_c4 <- comorbilidades_conteos[1,4]/c4
# 2.1.3.4.2 p( epoc | c4 )
p_epoc_c4 <- comorbilidades_conteos[2,4]/c4
# 2.1.3.4.3 p( asma | c4 )
p_asma_c4 <- comorbilidades_conteos[3,4]/c4
# 2.1.3.4.4 p( inmunsupr | c4 )
p_inmunsupr_c4 <- comorbilidades_conteos[4,4]/c4
# 2.1.3.4.5 p( hipertension | c4 )
p_hipertension_c4 <- comorbilidades_conteos[5,4]/c4
# 2.1.3.4.6 p( cardiovascular | c4 )
p_cardiovascular_c4 <- comorbilidades_conteos[6,4]/c4
# 2.1.3.4.7 p( obesidad | c4 )
p_obesidad_c4 <- comorbilidades_conteos[7,4]/c4
# 2.1.3.4.8 p( renal_cronica | c4 )
p_renal_cronica_c4 <- comorbilidades_conteos[8,4]/c4
# 2.1.3.4.9 p( tabaquismo | c4 )
p_tabaquismo_c4 <- comorbilidades_conteos[9,4]/c4
# 2.1.3.2.10 Se guardan las p´s en un vector 
p_comorb_c4 <- c(p_diab_c4,p_epoc_c4,p_asma_c4,p_inmunsupr_c4,
                 p_hipertension_c4,p_cardiovascular_c4,p_obesidad_c4,
                 p_renal_cronica_c4,p_tabaquismo_c4)

# 2.1.4 Se guarda como una matriz, todas las P ( COM_I & CAT_J ) obtenidas ---- 
#       previamente
matriz_comor <- matrix(c(p_comorb_c1,p_comorb_c2,p_comorb_c3,p_comorb_c4),
                       ncol = 4, byrow = F)
rownames(matriz_comor) <- c("p ( diabetes       | c_j )",
                            "p ( epoc           | c_j )",
                            "p ( asma           | c_j )", 
                            "p ( inmunsupr      | c_j )",
                            "p ( hipertension   | c_j )", 
                            "p ( cardiovascular | c_j )",
                            "p ( obesidad       | c_j )",
                            "p ( renal_cronica  | c_j )", 
                            "p ( tabaquismo     | c_j )")
colnames(matriz_comor) <-c("CATEGORIA 1", "CATEGORIA 2", "CATEGORIA 3", "CATEGORIA 4")
matriz_comor
# 2.1.4.1 Se guarda como archivo 
# save(matriz_comor, file = "03_Out/OutData/matriz_2_p_comorbilidades.RData")
#
#
# 2.1.4.2 Visualizacion de la matriz

# BiocManager::install("ComplexHeatmap")
# install_github("jokergoo/ComplexHeatmap")
mat = matriz_comor
rownames(mat) <- c("D",
                   "E",
                   "A", 
                   "I",
                   "H", 
                   "CV",
                   "O",
                   "RC", 
                   "T")
colnames(mat) <- c("< 18", "18 - 39",
                   "40 - 59", "60 <")
mat <- t(mat)

# jpeg("03_Out/Plots/heatmap_probabilidades_comorbilidades_categorias.jpg",
#      width = 5733, height = 5733, res = 800, units = "px")
Heatmap(mat,
        name= "Probabilidad",
        col = viridis(100),
        row_names_side = "left",
        cluster_rows = F,
        cluster_columns = F,
        row_title = "Grupos etarios",
        column_title = "Comorbilidades",
        column_names_side = "top",
        column_dend_side = "bottom",
        column_names_rot = 0,
        column_title_gp = gpar(fill = "gray", border = "gray",
                               fontsize = 9, fontface = "bold"),
        row_title_gp = gpar(fill = "gray", border = "gray",
                            fontsize = 9, fontface = "bold"),
        rect_gp = gpar(col = "white", lwd = 2),
        column_names_centered = T,
        row_names_centered = T)
# dev.off()

# 2.1.5 Determinacion de P ( COM_j ) ----
#       =         # TODOS LOS QUE TIENEN LA COM_j
#.        --- --- --- --- --- --- --- --- --- --- ---
#.          # Toda la poblacion de la base de datos
# 2.1.5.1 Conteo de comorbilidades de los casos TOTALES
# 2.1.5.1.1 Filtrar la base de datos TOTALES
casos_totales_re_comorbilidad <- select(casos_totales_re, 
                                        c(FECHA_SINTOMAS, EDAD, INTUBADO, NEUMONIA,
                                          DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
                                          CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA,
                                          TABAQUISMO, rango_de_edad))
# 2.1.5.1.2 Se agrega la columna de individuo 1 para sumar
casos_totales_re_comorbilidad <- mutate(casos_totales_re_comorbilidad, ind = 1)
# 2.1.5.1.3 Se hace el conteo con la funcion
com_totales <- comorbilidades_conteo(casos_totales_re_comorbilidad)
# 2.1.5.1.4 Se nombra a cada elemento del vector resultante con los conteos de casos 
#           con comorbilidad
names(com_totales) <- c("diabetes"         ,
                        "epoc"             ,
                        "asma"             ,
                        "inmunsupr"        ,
                        "hipertension"     ,
                        "cardiovascular"   ,
                        "obesidad"         ,
                        "renal_cronica"    ,
                        "tabaquismo"       )
# 2.1.5.2 Conteo de casos totales
n_total <- nrow(casos_totales_re_comorbilidad)
# 2.1.5.3 Determinacion de la probabilidad
# 2.1.5.3.1 P ( DIABETES )
p_diabetes <- com_totales[1]/n_total
# 2.1.5.3.2 P ( EPOC )
p_epoc <- com_totales[2]/n_total
# 2.1.5.3.3 P ( ASMA )
p_asma <- com_totales[3]/n_total
# 2.1.5.3.4 P ( INMUNSUPR )
p_inmunsupr <- com_totales[4]/n_total
# 2.1.5.3.5 P ( HIPERTENSION )
p_hipertension <- com_totales[5]/n_total
# 2.1.5.3.6 P ( CARDIOVASCULAR )
p_cardiovascular <- com_totales[6]/n_total
# 2.1.5.3.7 P ( OBESIDAD )
p_obesidad <- com_totales[7]/n_total
# 2.1.5.3.8 P ( RENAL_CRONICA )
p_renal_cronica <- com_totales[8]/n_total
# 2.1.5.3.9 P ( TABAQUISMO )
p_tabaquismo <- com_totales[9]/n_total


# 2.1.6 Determinacion de las PROBABILIDADES CONDICIONALES ----
# p( cat_i | com_j ) = P ( COM_j & CAT_i )
#                      --- --- --- --- ---
#                         P ( COM_j )
# 2.1.6.1 CATEGORIA 1 ===
# 2.1.6.1.1 p ( C1 | Diabetes ) 
p_c1_diabetes <- p_diab_c1 / p_diabetes
# 2.1.6.1.2 p ( C1 | EPOC ) 
p_c1_epoc <- p_epoc_c1 / p_epoc
# 2.1.6.1.3 p ( C1 | ASMA ) 
p_c1_asma <- p_asma_c1 / p_asma
# 2.1.6.1.4 p ( C1 | INMUNSUPR ) 
p_c1_inmunsupr <- p_inmunsupr_c1 / p_inmunsupr
# 2.1.6.1.5 p ( C1 | HIPERTENSION ) 
p_c1_hipertension <- p_hipertension_c1 / p_hipertension
# 2.1.6.1.6 p ( C1 | CARDIOVASCULAR ) 
p_c1_cardiovascular <-p_cardiovascular_c1 / p_cardiovascular
# 2.1.6.1.7 p ( C1 | OBESIDAD ) 
p_c1_obesidad <- p_obesidad_c1 / p_obesidad
# 2.1.6.1.8 p ( C1 | RENAL_CRONICA ) 
p_c1_renal_cronica <- p_renal_cronica_c1 / p_renal_cronica
# 2.1.6.1.9 p ( C1 | TABAQUISMO ) 
p_c1_tabaquismo <- p_tabaquismo_c1 / p_tabaquismo
# 2.1.6.1.10 Se guardan todas las p´s en un vector
p_c1_com <- c(p_c1_diabetes,p_c1_epoc,p_c1_asma,p_c1_inmunsupr,
              p_c1_hipertension,p_c1_cardiovascular,p_c1_obesidad,
              p_c1_renal_cronica,p_c1_tabaquismo)


# 2.1.6.2 CATEGORIA 2 ===
# 2.1.6.2.1 p ( C2 | Diabetes )
p_c2_diabetes <- p_diab_c2 / p_diabetes
# 2.1.6.2.2 p ( C2 | EPOC )
p_c2_epoc <- p_epoc_c2 / p_epoc
# 2.1.6.2.3 p ( C2 | ASMA )
p_c2_asma <- p_asma_c2 / p_asma
# 2.1.6.2.4 p ( C2 | INMUNSUPR ) 
p_c2_inmunsupr <- p_inmunsupr_c2 / p_inmunsupr
# 2.1.6.2.5 p ( C2 | HIPERTENSION ) 
p_c2_hipertension <- p_hipertension_c2 / p_hipertension
# 2.1.6.2.6 p ( C2 | CARDIOVASCULAR ) 
p_c2_cardiovascular <- p_cardiovascular_c2 / p_cardiovascular
# 2.1.6.2.7 p ( C2 | OBESIDAD ) 
p_c2_obesidad <- p_obesidad_c2 / p_obesidad
# 2.1.6.2.8 p ( C2 | RENAL_CRONICA ) 
p_c2_renal_cronica <- p_renal_cronica_c2 / p_renal_cronica
# 2.1.6.2.9 p ( C2 | TABAQUISMO ) 
p_c2_tabaquismo <- p_tabaquismo_c2 / p_tabaquismo
# 2.1.6.2.10 Se guardan todas las p´s en un vector
p_c2_com <- c(p_c2_diabetes,p_c2_epoc,p_c2_asma,p_c2_inmunsupr,
              p_c2_hipertension,p_c2_cardiovascular,p_c2_obesidad,
              p_c2_renal_cronica,p_c2_tabaquismo)


# 2.1.6.3 CATEGORIA 3 ===
# 2.1.6.3.1 p ( C3 | Diabetes )
p_c3_diabetes <- p_diab_c3 / p_diabetes
# 2.1.6.3.2 p ( C3 | EPOC )
p_c3_epoc <- p_epoc_c3 / p_epoc
# 2.1.6.3.3 p ( C3 | ASMA )
p_c3_asma <- p_asma_c3 / p_asma
# 2.1.6.3.4 p ( C3 | INMUNSUPR ) 
p_c3_inmunsupr <- p_inmunsupr_c3 / p_inmunsupr
# 2.1.6.3.5 p ( C3 | HIPERTENSION ) 
p_c3_hipertension <- p_hipertension_c3 / p_hipertension
# 2.1.6.3.6 p ( C3 | CARDIOVASCULAR ) 
p_c3_cardiovascular <- p_cardiovascular_c3 / p_cardiovascular
# 2.1.6.3.7 p ( C3 | OBESIDAD ) 
p_c3_obesidad <- p_obesidad_c3 / p_obesidad
# 2.1.6.3.8 p ( C3 | RENAL_CRONICA ) 
p_c3_renal_cronica <- p_renal_cronica_c3 / p_renal_cronica
# 2.1.6.3.9 p ( C3 | TABAQUISMO ) 
p_c3_tabaquismo <- p_tabaquismo_c3 / p_tabaquismo
# 2.1.6.3.10 Se guardan todas las p´s en un vector
p_c3_com <- c(p_c3_diabetes,p_c3_epoc,p_c3_asma,p_c3_inmunsupr,
              p_c3_hipertension,p_c3_cardiovascular,p_c3_obesidad,
              p_c3_renal_cronica,p_c3_tabaquismo)


# 2.1.6.4 CATEGORIA 3 ===
# 2.1.6.4.1 p ( C4 | Diabetes )
p_c4_diabetes <- p_diab_c4 / p_diabetes
# 2.1.6.4.2 p ( C4 | EPOC )
p_c4_epoc <- p_epoc_c4 / p_epoc
# 2.1.6.4.3 p ( C4 | ASMA )
p_c4_asma <- p_asma_c4 / p_asma
# 2.1.6.4.4 p ( C4 | INMUNSUPR ) 
p_c4_inmunsupr <- p_inmunsupr_c4 / p_inmunsupr
# 2.1.6.4.5 p ( C4 | HIPERTENSION ) 
p_c4_hipertension <- p_hipertension_c4 / p_hipertension
# 2.1.6.4.6 p ( C4 | CARDIOVASCULAR ) 
p_c4_cardiovascular <- p_cardiovascular_c4 / p_cardiovascular
# 2.1.6.4.7 p ( C4 | OBESIDAD ) 
p_c4_obesidad <- p_obesidad_c4 / p_obesidad
# 2.1.6.4.8 p ( C4 | RENAL_CRONICA ) 
p_c4_renal_cronica <- p_renal_cronica_c4 / p_renal_cronica
# 2.1.6.4.9 p ( C4 | TABAQUISMO ) 
p_c4_tabaquismo <- p_tabaquismo_c4 / p_tabaquismo
# 2.1.6.4.10 Se guardan todas las p´s en un vector
p_c4_com <- c(p_c4_diabetes,p_c4_epoc,p_c4_asma,p_c4_inmunsupr,
              p_c4_hipertension,p_c4_cardiovascular,p_c4_obesidad,
              p_c4_renal_cronica,p_c4_tabaquismo)


# 2.1.7 Se guardan todas las probabilidades como una MATRIZ DE PROBABILIDADES ---- 
#       DE COMORBILIDADES P ( CAT_I | COM_J )
matriz_p_comorbilidades <- matrix(c(p_c1_com,p_c2_com,p_c3_com,p_c4_com),
                                  ncol = 4, byrow = F)
colnames(matriz_p_comorbilidades) <- c("CATEGORIA 1", "CATEGORIA 2", 
                                       "CATEGORIA 3", "CATEGORIA 4")
rownames(matriz_p_comorbilidades) <- c("p ( cat_i | diabetes )",
                                       "p ( cat_i | epoc )",
                                       "p ( cat_i | asma )", 
                                       "p ( cat_i | inmunsupr )",
                                       "p ( cat_i | hipertension )", 
                                       "p ( cat_i | cardiovascular )",
                                       "p ( cat_i | obesidad )",
                                       "p ( cat_i | renal_cronica )", 
                                       "p ( cat_i | tabaquismo )")
matriz_p_comorbilidades
# 2.1.7.1 Se guarda la matriz como un archivo
# save(matriz_p_comorbilidades, file = "03_Out/OutData/matriz_p_comorbilidades.RData")
#
#
# 2.1.7.2 Visualizacion en un Heatmap
mat_2 = matriz_p_comorbilidades
rownames(mat_2) <- c("Diabetes",
                   "EPOC",
                   "Asma", 
                   "Inmunsupr",
                   "Hipertension", 
                   "Cardiovascular",
                   "Obesidad",
                   "Renal Crónica", 
                   "Tabaquismo")
Heatmap(mat_2, name= "p(com|cat)")

# 2.2 Conteo de casos positivos que tienen al menos una comorbilidad ----
# 2.2.1 Categoria 1: menores de 18 años
#       Se toma cada renglon de la base de datos, que representa a un individuo 
#       y con la funcion condicional if(), se busca por cada renglon aquellos que
#       tengan al menos una comorbilidad, dentro de un ciclo for(). Se usa una funcion
conteo_una_com_cat1 <- conteo_una_comorbilidad(casos_pos_re_comorbilidad_cat_1)
#
conteo_una_com_cat1
# 2.2.2 Categoria 2: 18 a 39 años
#       Se toma cada renglon de la base de datos, que representa a un individuo 
#       y con la funcion condicional if(), se busca por cada renglon aquellos que
#       tengan al menos una comorbilidad, dentro de un ciclo for(). Se usa una funcion
conteo_una_com_cat2 <- conteo_una_comorbilidad(casos_pos_re_comorbilidad_cat_2)
#
conteo_una_com_cat2
# 2.2.3 Categoria 3: 40 a 59 años
#       Se toma cada renglon de la base de datos, que representa a un individuo 
#       y con la funcion condicional if(), se busca por cada renglon aquellos que
#       tengan al menos una comorbilidad, dentro de un ciclo for(). Se usa una funcion
conteo_una_com_cat3 <- conteo_una_comorbilidad(casos_pos_re_comorbilidad_cat_3)
#
conteo_una_com_cat3
# 2.2.4 Categoria 4: mayores de 60 años
#       Se toma cada renglon de la base de datos, que representa a un individuo 
#       y con la funcion condicional if(), se busca por cada renglon aquellos que
#       tengan al menos una comorbilidad, dentro de un ciclo for(). Se usa una funcion
conteo_una_com_cat4 <- conteo_una_comorbilidad(casos_pos_re_comorbilidad_cat_4)
#
conteo_una_com_cat4
# 2.2.4.1 Vector de conteos
conteo_una_com_all_cat <- c(conteo_una_com_cat1, conteo_una_com_cat2, 
                            conteo_una_com_cat3, conteo_una_com_cat4)
# 2.2.5. Normalizacion de los datos de conteo
# 
min_val <- min(conteo_una_com_all_cat)
max_val <- max(conteo_una_com_all_cat)
normalized_vector <- (conteo_una_com_all_cat - min_val) / (max_val - min_val)
# 2.2.5.1. Categoria 1: menores de 18 años
nomc1 <- conteo_una_com_cat1/c1
nomc1
# 2.2.5.2. Categoria 2:18 a 39 años
nomc2 <- conteo_una_com_cat2/c2
nomc2
# 2.2.5.3. Categoria 3: 40 a 59 años
nomc3 <- conteo_una_com_cat3/c3
nomc3
# 2.2.5.4. Categoria 4: mayores de 60 años
nomc4 <- conteo_una_com_cat4/c4
nomc4
#
# 2.2.6 Visualicacion de los datos de conteo
conteos_normalizados <- c(nomc1,nomc2,nomc3,nomc4)
names(conteos_normalizados) <- c(">18","18-39","40-59","60>")
mat_3 <- conteos_normalizados
col_fun <- colorRamp2(c( 0, 0.375, 0.75), c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_p_una_comorbilidad <- Heatmap(mat_3, 
                                      cluster_rows = F,
                                      show_column_names = F,
                                      col = col_fun,
                                      name = "Probabilidad",
                                      row_order = order(names(mat_3),
                                                        decreasing = T),
                                      row_names_gp = gpar(fontsize = 9, 
                                                          face = "bold"),
                                      row_title = "Grupos etarios",
                                      row_title_gp = gpar(fill = "gray", border = "gray",
                                                          fontsize = 9, fontface = "bold"),
                                      row_names_centered = T,
                                      row_names_side = "left",
                                      column_names_rot = 0)
heatmap_p_una_comorbilidad

# jpeg("03_Out/Plots/heatmap_p_una_comorbilidad.jpeg",
#     width = 5733, height = 5733, res = 800, units = "px")
# heatmap_p_una_comorbilidad
# dev.off()

mat_3 <- as.matrix(mat_3)
col_fun <- colorRamp2(c( 0, 0.45, 0.9), 
                      c("lightblue", "deepskyblue", "deepskyblue4"))
heatmap_p_una_comorbilidad_v2 <-  Heatmap(mat_3, 
                                          col = col_fun,
                                          rect_gp = gpar(type = "none"),
                                          cell_fun = function(j, i, x, y, width, height, fill) {
                                                  
                                                  grid.rect(x = x, y = y, width = width, height = height, 
                                                            gp = gpar(col = "black", fill = NA))
                                                  if(i >= j) {
                                                          grid.circle(x = x, y = y, 
                                                                      r = abs(mat_3[i, j])/1.6 * min(unit.c(width, height)), 
                                                                      gp = gpar(fill = col_fun(mat_3[i, j]), col = NA))
                                                  }
                                          },
                                          cluster_rows = FALSE, 
                                          cluster_columns = FALSE,
                                          show_row_names = T, 
                                          show_column_names = F,
                                          name = "Valor", 
                                          row_names_side = "left")
heatmap_p_una_comorbilidad_v2

# jpeg("03_Out/Plots/heatmap_p_una_comorbilidad_v2.jpeg",
#     width = 4300, height = 4733, res = 800, units = "px")
# heatmap_p_una_comorbilidad_v2
# dev.off()
