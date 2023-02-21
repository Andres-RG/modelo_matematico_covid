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


# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")


#¿Cuál es la probabilidad que una persona de cierta tenga una comorbilidad, dos,
#tres , cuatro o combinaciones de éstas ?

#  p de que dado que tienen una comorbilidad, pertenencen a la categoría 
#  p( cat_j | com_i )

# 1 Seleccionar solo las columnas de interes 
casos_pos_re_comorbilidad <- select(casos_positivos_re, 
                                    c(FECHA_SINTOMAS, EDAD, INTUBADO, NEUMONIA,
                                      DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, 
                                      CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, 
                                      TABAQUISMO, rango_de_edad))
casos_pos_re_comorbilidad <- arrange(casos_pos_re_comorbilidad, rango_de_edad)
casos_pos_re_comorbilidad <- mutate(casos_pos_re_comorbilidad, ind = 1)
# save(casos_pos_re_comorbilidad, file = "03_Out/OutData/casos_positivos_re_comorbilidad.RData")

# cat 1. Menores de 18 años
casos_pos_re_comorbilidad_cat_1 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-")
comorbilidades_casos_1 <- t(comorbilidades_casos_1)


# cat 2. 18 a 39 años
casos_pos_re_comorbilidad_cat_2 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-29" | rango_de_edad == "30-39")
comorbilidades_casos_2 <- t(comorbilidades_casos_2)


# cat 3. 40 a 59 años
casos_pos_re_comorbilidad_cat_3 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "40-49" | rango_de_edad == "50-59")
comorbilidades_casos_3 <- t(comorbilidades_casos_3)


# cat 4. 60 años en adelante
casos_pos_re_comorbilidad_cat_4 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "60-69" | rango_de_edad == "70+")
comorbilidades_casos_4 <- t(comorbilidades_casos_4)

# 2 Determinacion de probabilidad
# p( cat_i | com_j ) = ( p ( com_j | cat_1 ) * p( com_j ) ) / (.p( com_j ) * p ( com_j | cat_1 ) + (1-p( com_j )) * (1-p ( com_j | cat_1 )) )

### p( com_j | cat_i )

com_cat_1 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_1)
com_cat_2 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_2)
com_cat_3 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_3)
com_cat_4 <- comorbilidades_conteo(casos_pos_re_comorbilidad_cat_4)


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
# save(comorbilidades_conteos, file = "03_Out/OutData/conteo_comorbilidades.RData")


# SIN COMORBILIDADES *******===

sin_com_cat_1 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_1)
sin_com_cat_2 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_2)
sin_com_cat_3 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_3)
sin_com_cat_4 <- sin_comorbilidades_conteo(casos_pos_re_comorbilidad_cat_4)


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
# save(comorbilidades_conteos, file = "03_Out/OutData/conteo_SIN_comorbilidades.RData")


#       # # # C 1
c1 <- sum(casos_pos_re_comorbilidad_cat_1$ind)
#       # # # C 2
c2 <- sum(casos_pos_re_comorbilidad_cat_2$ind)
#       # # # C 3
c3 <- sum(casos_pos_re_comorbilidad_cat_3$ind)
#       # # # C 4
c4 <- sum(casos_pos_re_comorbilidad_cat_4$ind)

N <- c1+c2+c3+c4

### =========== CATEGORIA 1 ---------
### p( diabetes | c1 )
p_diab_c1 <- comorbilidades_conteos[1,1]/c1

### p( epoc | c1 )
p_epoc_c1 <- comorbilidades_conteos[2,1]/c1

### p( asma | c1 )
p_asma_c1 <- comorbilidades_conteos[3,1]/c1

### p( inmunsupr | c1 )
p_inmunsupr_c1 <- comorbilidades_conteos[4,1]/c1

### p( hipertension | c1 )
p_hipertension_c1 <- comorbilidades_conteos[5,1]/c1

### p( cardiovascular | c1 )
p_cardiovascular_c1 <- comorbilidades_conteos[6,1]/c1

### p( obesidad | c1 )
p_obesidad_c1 <- comorbilidades_conteos[7,1]/c1

### p( renal_cronica | c1 )
p_renal_cronica_c1 <- comorbilidades_conteos[8,1]/c1

### p( tabaquismo | c1 )
p_tabaquismo_c1 <- comorbilidades_conteos[9,1]/c1

p_comorb_c1 <- c(p_diab_c1,p_epoc_c1,p_asma_c1,p_inmunsupr_c1,
                 p_hipertension_c1,p_cardiovascular_c1,p_obesidad_c1,
                 p_renal_cronica_c1,p_tabaquismo_c1)


### =========== CATEGORIA 2 ---------

### p( diabetes | c2 )
p_diab_c2 <- comorbilidades_conteos[1,2]/c2

### p( epoc | c2 )
p_epoc_c2 <- comorbilidades_conteos[2,2]/c2

### p( asma | c2 )
p_asma_c2 <- comorbilidades_conteos[3,2]/c2

### p( inmunsupr | c2 )
p_inmunsupr_c2 <- comorbilidades_conteos[4,2]/c2

### p( hipertension | c2 )
p_hipertension_c2 <- comorbilidades_conteos[5,2]/c2

### p( cardiovascular | c2 )
p_cardiovascular_c2 <- comorbilidades_conteos[6,2]/c2

### p( obesidad | c2 )
p_obesidad_c2 <- comorbilidades_conteos[7,2]/c2

### p( renal_cronica | c2 )
p_renal_cronica_c2 <- comorbilidades_conteos[8,2]/c2

### p( tabaquismo | c2 )
p_tabaquismo_c2 <- comorbilidades_conteos[9,2]/c2

p_comorb_c2 <- c(p_diab_c2,p_epoc_c2,p_asma_c2,p_inmunsupr_c2,
                 p_hipertension_c2,p_cardiovascular_c2,p_obesidad_c2,
                 p_renal_cronica_c2,p_tabaquismo_c2)


### =========== CATEGORIA 3 ---------

### p( diabetes | c3 )
p_diab_c3 <- comorbilidades_conteos[1,3]/c3

### p( epoc | c3 )
p_epoc_c3 <- comorbilidades_conteos[2,3]/c3

### p( asma | c3 )
p_asma_c3 <- comorbilidades_conteos[3,3]/c3

### p( inmunsupr | c3 )
p_inmunsupr_c3 <- comorbilidades_conteos[4,3]/c3

### p( hipertension | c3 )
p_hipertension_c3 <- comorbilidades_conteos[5,3]/c3

### p( cardiovascular | c3 )
p_cardiovascular_c3 <- comorbilidades_conteos[6,3]/c3

### p( obesidad | c3 )
p_obesidad_c3 <- comorbilidades_conteos[7,3]/c3

### p( renal_cronica | c3 )
p_renal_cronica_c3 <- comorbilidades_conteos[8,3]/c3

### p( tabaquismo | c3 )
p_tabaquismo_c3 <- comorbilidades_conteos[9,3]/c3

p_comorb_c3 <- c(p_diab_c3,p_epoc_c3,p_asma_c3,p_inmunsupr_c3,
                 p_hipertension_c3,p_cardiovascular_c3,p_obesidad_c3,
                 p_renal_cronica_c3,p_tabaquismo_c3)


### =========== CATEGORIA 4 ---------

### p( diabetes | c4 )
p_diab_c4 <- comorbilidades_conteos[1,4]/c4

### p( epoc | c4 )
p_epoc_c4 <- comorbilidades_conteos[2,4]/c4

### p( asma | c4 )
p_asma_c4 <- comorbilidades_conteos[3,4]/c4

### p( inmunsupr | c4 )
p_inmunsupr_c4 <- comorbilidades_conteos[4,4]/c4

### p( hipertension | c4 )
p_hipertension_c4 <- comorbilidades_conteos[5,4]/c4

### p( cardiovascular | c4 )
p_cardiovascular_c4 <- comorbilidades_conteos[6,4]/c4

### p( obesidad | c4 )
p_obesidad_c4 <- comorbilidades_conteos[7,4]/c4

### p( renal_cronica | c4 )
p_renal_cronica_c4 <- comorbilidades_conteos[8,4]/c4

### p( tabaquismo | c4 )
p_tabaquismo_c4 <- comorbilidades_conteos[9,4]/c4

p_comorb_c4 <- c(p_diab_c4,p_epoc_c4,p_asma_c4,p_inmunsupr_c4,
                 p_hipertension_c4,p_cardiovascular_c4,p_obesidad_c4,
                 p_renal_cronica_c4,p_tabaquismo_c4)


#### ===== P ( COM_I | CAT_J ) -------
matriz_comor <- matrix(c(p_comorb_c1,p_comorb_c2,p_comorb_c3,p_comorb_c4),
                       ncol = 4, byrow = F)
rownames(matriz_comor) <- c("p ( diabetes | c_j )"," p ( epoc | c_j )",
                            "p ( asma | c_j )", "p ( inmunsupr | c_j )",
                            "p ( hipertension | c_j )", "p ( cardiovascular | c_j )",
                            "p ( obesidad | c_j )",
                            "p ( renal_cronica | c_j )", "p ( tabaquismo | c_j )")
colnames(matriz_comor) <-c("CATEGORIA 1", "CATEGORIA 2", "CATEGORIA 3", "CATEGORIA 4")
matriz_comor




### p ( com_j )
### p ( com_j ) = #TODOS LOS QUE TIENEN COM_J / N
#### P ( DIABETES )
p_diabetes <- sum(comorbilidades_conteos[1,])/(N)
#### P ( EPOC )
p_epoc <- sum(comorbilidades_conteos[2,])/(N)
#### P ( ASMA )
p_asma <- sum(comorbilidades_conteos[3,])/(N)
#### P ( INMUNSUPR )
p_inmunsupr <- sum(comorbilidades_conteos[4,])/(N)
#### P ( HIPERTENSION )
p_hipertension <- sum(comorbilidades_conteos[5,])/(N)
#### P ( CARDIOVASCULAR )
p_cardiovascular <- sum(comorbilidades_conteos[6,])/(N)
#### P ( OBESIDAD )
p_obesidad <- sum(comorbilidades_conteos[7,])/(N)
#### P ( RENAL_CRONICA )
p_renal_cronica <- sum(comorbilidades_conteos[8,])/(N)
#### P ( TABAQUISMO )
p_tabaquismo <- sum(comorbilidades_conteos[9,])/(N)
