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
library(randomcoloR)

# Se carga la base de datos
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

# 1 Seleccionar solo las columnas de interes con comorbilidades
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
comorbilidades_casos_1 <- t(comorbilidades_casos_1)

# 1.4.2 cat 2. 18 a 39 años
casos_pos_re_comorbilidad_cat_2 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-29" | rango_de_edad == "30-39")
comorbilidades_casos_2 <- t(comorbilidades_casos_2)

# 1.4.3 cat 3. 40 a 59 años
casos_pos_re_comorbilidad_cat_3 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "40-49" | rango_de_edad == "50-59")
comorbilidades_casos_3 <- t(comorbilidades_casos_3)

# 1.4.4 cat 4. 60 años en adelante
casos_pos_re_comorbilidad_cat_4 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "60-69" | rango_de_edad == "70+")
comorbilidades_casos_4 <- t(comorbilidades_casos_4)

# 2 Determinacion de probabilidad
# p( cat_i | com_j ) = ( p ( com_j | cat_1 ) * p( com_j ) ) / 
#                      (.p( com_j ) * p ( com_j | cat_1 ) + (1-p( com_j )) * 
#                      (1-p ( com_j | cat_1 )) )

# 2.1 Obtención de p( com_j | cat_i )
# 2.1.1 Conteo de casos con comorbilidades

# 2.1.1.1 Se obtienen los conteos por categoria de cuantos pacientes tienen alguna
#         comorbilidad, se hace por cada una, y para cada categoria.
#         *Se hace uso de una funcion.
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


# 2.1.2 Conteo de individuos por categoria
# 2.1.2.1      CATEGORIA 1: MENORES DE 18 AÑOS
c1 <- sum(casos_pos_re_comorbilidad_cat_1$ind)
# 2.1.2.2      CATEGIRIA 2: 18 - 39 AÑOS
c2 <- sum(casos_pos_re_comorbilidad_cat_2$ind)
# 2.1.2.3      CATEGORIA 3: 40 - 59 AÑOS
c3 <- sum(casos_pos_re_comorbilidad_cat_3$ind)
# 2.1.2.4      CATEOGIRA 4: 60 AÑOS EN ADELANTE
c4 <- sum(casos_pos_re_comorbilidad_cat_4$ind)
# 2.1.2.5      TOTAL DE LA POBLACION/CASOS REGISTRADOS
N <- c1+c2+c3+c4


# 2.1.3 Determinación de la probabilidad
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


# 2.1.3.4 ---- CATEGORIA 3
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


# 2.1.4 Se guarda como una matriz, todas las P ( COM_I | CAT_J ) obtenidas 
#       previamente
matriz_comor <- matrix(c(p_comorb_c1,p_comorb_c2,p_comorb_c3,p_comorb_c4),
                       ncol = 4, byrow = F)
rownames(matriz_comor) <- c("p ( diabetes | c_j )",
                            "p ( epoc | c_j )",
                            "p ( asma | c_j )", 
                            "p ( inmunsupr | c_j )",
                            "p ( hipertension | c_j )", 
                            "p ( cardiovascular | c_j )",
                            "p ( obesidad | c_j )",
                            "p ( renal_cronica | c_j )", 
                            "p ( tabaquismo | c_j )")
colnames(matriz_comor) <-c("CATEGORIA 1", "CATEGORIA 2", "CATEGORIA 3", "CATEGORIA 4")
matriz_comor


# 2.1.5 Determinacion de P ( COM_J )
#       = #TODOS LOS QUE TIENEN LA COM_J / N
# 2.1.5.1 P ( DIABETES )
p_diabetes <- sum(comorbilidades_conteos[1,])/(N)
# 2.1.5.2 P ( EPOC )
p_epoc <- sum(comorbilidades_conteos[2,])/(N)
# 2.1.5.3 P ( ASMA )
p_asma <- sum(comorbilidades_conteos[3,])/(N)
# 2.1.5.4 P ( INMUNSUPR )
p_inmunsupr <- sum(comorbilidades_conteos[4,])/(N)
# 2.1.5.5 P ( HIPERTENSION )
p_hipertension <- sum(comorbilidades_conteos[5,])/(N)
# 2.1.5.6 P ( CARDIOVASCULAR )
p_cardiovascular <- sum(comorbilidades_conteos[6,])/(N)
# 2.1.5.7 P ( OBESIDAD )
p_obesidad <- sum(comorbilidades_conteos[7,])/(N)
# 2.1.5.8 P ( RENAL_CRONICA )
p_renal_cronica <- sum(comorbilidades_conteos[8,])/(N)
# 2.1.5.9 P ( TABAQUISMO )
p_tabaquismo <- sum(comorbilidades_conteos[9,])/(N)


# 2.1.6 Determinacion de las PROBABILIDADES CONDICIONALES
# p( cat_i | com_j ) = ( p ( com_j | cat_1 ) * p( com_j ) ) / 
#                      (.p( com_j ) * p ( com_j | cat_1 ) + (1-p( com_j )) * 
#                      (1-p ( com_j | cat_1 )) )
# 2.1.6.1 CATEGORIA 1 ===
# 2.1.6.1.1 p ( C1 | Diabetes ) 
p_c1_diabetes <-( p_diab_c1 * p_diabetes )/
    ( p_diabetes * p_diab_c1 + ((1-p_diabetes) * (1-p_diab_c1)))
# 2.1.6.1.2 p ( C1 | EPOC ) 
p_c1_epoc <-( p_epoc_c1 * p_epoc )/
    ( p_epoc * p_epoc_c1 + ((1-p_epoc) * (1-p_epoc_c1)))
# 2.1.6.1.3 p ( C1 | ASMA ) 
p_c1_asma <-( p_asma_c1 * p_asma )/
    ( p_asma * p_asma_c1 + ((1-p_asma) * (1-p_asma_c1)))
# 2.1.6.1.4 p ( C1 | INMUNSUPR ) 
p_c1_inmunsupr <-( p_inmunsupr_c1 * p_inmunsupr )/
    ( p_inmunsupr * p_inmunsupr_c1 + ((1-p_inmunsupr) * (1-p_inmunsupr_c1)))
# 2.1.6.1.5 p ( C1 | HIPERTENSION ) 
p_c1_hipertension <-( p_hipertension_c1 * p_hipertension )/
    ( p_hipertension * p_hipertension_c1 + ((1-p_hipertension) * (1-p_hipertension_c1)))
# 2.1.6.1.6 p ( C1 | CARDIOVASCULAR ) 
p_c1_cardiovascular <-( p_cardiovascular_c1 * p_cardiovascular )/
    ( p_cardiovascular * p_cardiovascular_c1 + ((1-p_cardiovascular) * (1-p_cardiovascular_c1)))
# 2.1.6.1.7 p ( C1 | OBESIDAD ) 
p_c1_obesidad <-( p_obesidad_c1 * p_obesidad )/
    ( p_obesidad * p_obesidad_c1 + ((1-p_obesidad) * (1-p_obesidad_c1)))
# 2.1.6.1.8 p ( C1 | RENAL_CRONICA ) 
p_c1_renal_cronica <-( p_renal_cronica_c1 * p_renal_cronica )/
    ( p_renal_cronica * p_renal_cronica_c1 + ((1-p_renal_cronica) * (1-p_renal_cronica_c1)))
# 2.1.6.1.9 p ( C1 | TABAQUISMO ) 
p_c1_tabaquismo <-( p_tabaquismo_c1 * p_tabaquismo )/
    ( p_tabaquismo * p_tabaquismo_c1 + ((1-p_tabaquismo) * (1-p_tabaquismo_c1)))
# 2.1.6.1.10 Se guardan todas las p´s en un vector
p_c1_com <- c(p_c1_diabetes,p_c1_epoc,p_c1_asma,p_c1_inmunsupr,
              p_c1_hipertension,p_c1_cardiovascular,p_c1_obesidad,
              p_c1_renal_cronica,p_c1_tabaquismo)


# 2.1.6.2 CATEGORIA 2 ===
# 2.1.6.2.1 p ( C2 | Diabetes ) 
p_c2_diabetes <-( p_diab_c2 * p_diabetes )/
    ( p_diabetes * p_diab_c2 + ((1-p_diabetes) * (1-p_diab_c2)))
# 2.1.6.2.2 p ( C2 | EPOC ) 
p_c2_epoc <-( p_epoc_c2 * p_epoc )/
    ( p_epoc * p_epoc_c2 + ((1-p_epoc) * (1-p_epoc_c2)))
# 2.1.6.2.3 p ( C2 | ASMA ) 
p_c2_asma <-( p_asma_c2 * p_asma )/
    ( p_asma * p_asma_c2 + ((1-p_asma) * (1-p_asma_c2)))
# 2.1.6.2.4 p ( C2 | INMUNSUPR ) 
p_c2_inmunsupr <-( p_inmunsupr_c2 * p_inmunsupr )/
    ( p_inmunsupr * p_inmunsupr_c2 + ((1-p_inmunsupr) * (1-p_inmunsupr_c2)))
# 2.1.6.2.5 p ( C2 | HIPERTENSION ) 
p_c2_hipertension <-( p_hipertension_c2 * p_hipertension )/
    ( p_hipertension * p_hipertension_c2 + ((1-p_hipertension) * (1-p_hipertension_c2)))
# 2.1.6.2.6 p ( C2 | CARDIOVASCULAR ) 
p_c2_cardiovascular <-( p_cardiovascular_c2 * p_cardiovascular )/
    ( p_cardiovascular * p_cardiovascular_c2 + ((1-p_cardiovascular) * (1-p_cardiovascular_c2)))
# 2.1.6.2.7 p ( C2 | OBESIDAD ) 
p_c2_obesidad <-( p_obesidad_c2 * p_obesidad )/
    ( p_obesidad * p_obesidad_c2 + ((1-p_obesidad) * (1-p_obesidad_c2)))
# 2.1.6.2.8 p ( C2 | RENAL_CRONICA ) 
p_c2_renal_cronica <-( p_renal_cronica_c2 * p_renal_cronica )/
    ( p_renal_cronica * p_renal_cronica_c2 + ((1-p_renal_cronica) * (1-p_renal_cronica_c2)))
# 2.1.6.2.9 p ( C2 | TABAQUISMO ) 
p_c2_tabaquismo <-( p_tabaquismo_c2 * p_tabaquismo )/
    ( p_tabaquismo * p_tabaquismo_c2 + ((1-p_tabaquismo) * (1-p_tabaquismo_c2)))
# 2.1.6.2.10 Se guardan todas las p´s en un vector
p_c2_com <- c(p_c2_diabetes,p_c2_epoc,p_c2_asma,p_c2_inmunsupr,
              p_c2_hipertension,p_c2_cardiovascular,p_c2_obesidad,
              p_c2_renal_cronica,p_c2_tabaquismo)


# 2.1.6.3 CATEGORIA 3 ===
# 2.1.6.3.1 p ( C3 | Diabetes ) 
p_c3_diabetes <-( p_diab_c3 * p_diabetes )/
    ( p_diabetes * p_diab_c3 + ((1-p_diabetes) * (1-p_diab_c3)))
# 2.1.6.3.2 p ( C3 | EPOC ) 
p_c3_epoc <-( p_epoc_c3 * p_epoc )/
    ( p_epoc * p_epoc_c3 + ((1-p_epoc) * (1-p_epoc_c3)))
# 2.1.6.3.3 p ( C3 | ASMA ) 
p_c3_asma <-( p_asma_c3 * p_asma )/
    ( p_asma * p_asma_c3 + ((1-p_asma) * (1-p_asma_c3)))
# 2.1.6.3.4 p ( C3 | INMUNSUPR ) 
p_c3_inmunsupr <-( p_inmunsupr_c3 * p_inmunsupr )/
    ( p_inmunsupr * p_inmunsupr_c3 + ((1-p_inmunsupr) * (1-p_inmunsupr_c3)))
# 2.1.6.3.5 p ( C3 | HIPERTENSION ) 
p_c3_hipertension <-( p_hipertension_c3 * p_hipertension )/
    ( p_hipertension * p_hipertension_c3 + ((1-p_hipertension) * (1-p_hipertension_c3)))
# 2.1.6.3.6 p ( C3 | CARDIOVASCULAR ) 
p_c3_cardiovascular <-( p_cardiovascular_c3 * p_cardiovascular )/
    ( p_cardiovascular * p_cardiovascular_c3 + ((1-p_cardiovascular) * (1-p_cardiovascular_c3)))
# 2.1.6.3.7 p ( C3 | OBESIDAD ) 
p_c3_obesidad <-( p_obesidad_c3 * p_obesidad )/
    ( p_obesidad * p_obesidad_c3 + ((1-p_obesidad) * (1-p_obesidad_c3)))
# 2.1.6.3.8 p ( C3 | RENAL_CRONICA ) 
p_c3_renal_cronica <-( p_renal_cronica_c3 * p_renal_cronica )/
    ( p_renal_cronica * p_renal_cronica_c3 + ((1-p_renal_cronica) * (1-p_renal_cronica_c3)))
# 2.1.6.3.9 p ( C3 | TABAQUISMO ) 
p_c3_tabaquismo <-( p_tabaquismo_c3 * p_tabaquismo )/
    ( p_tabaquismo * p_tabaquismo_c3 + ((1-p_tabaquismo) * (1-p_tabaquismo_c3)))
# 2.1.6.3.10 Se guardan todas las p´s en un vector
p_c3_com <- c(p_c3_diabetes,p_c3_epoc,p_c3_asma,p_c3_inmunsupr,
              p_c3_hipertension,p_c3_cardiovascular,p_c3_obesidad,
              p_c3_renal_cronica,p_c3_tabaquismo)


# 2.1.6.4 CATEGORIA 3 ===
# 2.1.6.4.1 p ( C4 | Diabetes ) 
p_c4_diabetes <-( p_diab_c4 * p_diabetes )/
    ( p_diabetes * p_diab_c4 + ((1-p_diabetes) * (1-p_diab_c4)))
# 2.1.6.4.2 p ( C4 | EPOC ) 
p_c4_epoc <-( p_epoc_c4 * p_epoc )/
    ( p_epoc * p_epoc_c4 + ((1-p_epoc) * (1-p_epoc_c4)))
# 2.1.6.4.3 p ( C4 | ASMA ) 
p_c4_asma <-( p_asma_c4 * p_asma )/
    ( p_asma * p_asma_c4 + ((1-p_asma) * (1-p_asma_c4)))
# 2.1.6.4.4 p ( C4 | INMUNSUPR ) 
p_c4_inmunsupr <-( p_inmunsupr_c4 * p_inmunsupr )/
    ( p_inmunsupr * p_inmunsupr_c4 + ((1-p_inmunsupr) * (1-p_inmunsupr_c4)))
# 2.1.6.4.5 p ( C4 | HIPERTENSION ) 
p_c4_hipertension <-( p_hipertension_c4 * p_hipertension )/
    ( p_hipertension * p_hipertension_c4 + ((1-p_hipertension) * (1-p_hipertension_c4)))
# 2.1.6.4.6 p ( C4 | CARDIOVASCULAR ) 
p_c4_cardiovascular <-( p_cardiovascular_c4 * p_cardiovascular )/
    ( p_cardiovascular * p_cardiovascular_c4 + ((1-p_cardiovascular) * (1-p_cardiovascular_c4)))
# 2.1.6.4.7 p ( C4 | OBESIDAD ) 
p_c4_obesidad <-( p_obesidad_c4 * p_obesidad )/
    ( p_obesidad * p_obesidad_c4 + ((1-p_obesidad) * (1-p_obesidad_c4)))
# 2.1.6.4.8 p ( C4 | RENAL_CRONICA ) 
p_c4_renal_cronica <-( p_renal_cronica_c4 * p_renal_cronica )/
    ( p_renal_cronica * p_renal_cronica_c4 + ((1-p_renal_cronica) * (1-p_renal_cronica_c4)))
# 2.1.6.4.9 p ( C4 | TABAQUISMO ) 
p_c4_tabaquismo <-( p_tabaquismo_c4 * p_tabaquismo )/
    ( p_tabaquismo * p_tabaquismo_c4 + ((1-p_tabaquismo) * (1-p_tabaquismo_c4)))
# 2.1.6.4.10 Se guardan todas las p´s en un vector
p_c4_com <- c(p_c4_diabetes,p_c4_epoc,p_c4_asma,p_c4_inmunsupr,
              p_c4_hipertension,p_c4_cardiovascular,p_c4_obesidad,
              p_c4_renal_cronica,p_c4_tabaquismo)


# 2.1.7 Se guardan todas las probabilidades como una MATRIZ DE PROBABILIDADES 
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