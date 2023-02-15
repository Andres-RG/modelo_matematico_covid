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
                                      OTRA_COM, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, 
                                      TABAQUISMO, OTRO_CASO, rango_de_edad))
casos_pos_re_comorbilidad <- arrange(casos_pos_re_comorbilidad, rango_de_edad)
# save(casos_pos_re_comorbilidad, file = "03_Out/OutData/casos_positivos_re_comorbilidad.RData")

# Ordenar por rango de edad
comorbilidades_casos <- casos_pos_re_comorbilidad[,c(5:15)]

# cat 1. Menores de 18 años
casos_pos_re_comorbilidad_cat_1 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == "18-")
comorbilidades_casos_1 <- casos_pos_re_comorbilidad_cat_1[,c(5:15)]
comorbilidades_casos_1 <- t(comorbilidades_casos_1)


# cat 2. 18 a 39 años
casos_pos_re_comorbilidad_cat_2 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("18-29", "30-39"))
comorbilidades_casos_2 <- casos_pos_re_comorbilidad_cat_2[,c(5:15)]
comorbilidades_casos_2 <- t(comorbilidades_casos_2)


# cat 3. 40 a 59 años
casos_pos_re_comorbilidad_cat_3 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("40-49", "50-59"))
comorbilidades_casos_3 <- casos_pos_re_comorbilidad_cat_3[,c(5:15)]
comorbilidades_casos_3 <- t(comorbilidades_casos_3)


# cat 4. 60 años en adelante
casos_pos_re_comorbilidad_cat_4 <- filter(casos_pos_re_comorbilidad, 
                                          rango_de_edad == c("60-69", "70+"))
comorbilidades_casos_4 <- casos_pos_re_comorbilidad_cat_4[,c(5:15)]
comorbilidades_casos_4 <- t(comorbilidades_casos_4)

# 2 Determinacion de probabilidad
# p( cat_1 | com ) = (SUM com / N) / P( cat_1 )

## 2 . 1   p ( com n cat ) 

comorbilidades <- comorbilidadesdet(comorbilidades_casos_1)







