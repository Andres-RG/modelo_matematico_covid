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

# Ordenar por rango de edad
comorbilidades_casos <- casos_pos_re_comorbilidad[,c(5:13)]

# cat 1. Menores de 18 años
casos_pos_re_comorbilidad_cat_1 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-")
comorbilidades_casos_1 <- t(comorbilidades_casos_1)


# cat 2. 18 a 39 años
casos_pos_re_comorbilidad_cat_2 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("18-29", "30-39"))
comorbilidades_casos_2 <- t(comorbilidades_casos_2)


# cat 3. 40 a 59 años
casos_pos_re_comorbilidad_cat_3 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("40-49", "50-59"))
comorbilidades_casos_3 <- t(comorbilidades_casos_3)


# cat 4. 60 años en adelante
casos_pos_re_comorbilidad_cat_4 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("60-69", "70+"))
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
