# Librerias necesarias 

library(tidyverse)
library(deSolve)
library(ape)
library(lubridate)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se carga la base de datos
load("01_RawData/datos_covid_qro.RData")

# 1. Ragos de edad =============================================================

#    De la base de datos inicial, se determinaran los rangos de edad de todos
#    los casos positivos a Covid-19 hasta 17 de octubre, 2021. A partir de esta
#    adicion de datos, se construye la grafica para casos positivos divididos
#    por rangos de edades. Este objeto contendra todos los casos, con la columna
#    de rango de edades, de cada individuo. Se hace uso de una función
#    desarrollada.

# casos_totales_re <- mutate(datos_covid_qro, rango_de_edad = rangos_edades(datos_covid_qro$EDAD))

#    Se guarda el objeto como un objeto .RData
# save(casos_totales_re, file = "03_Out/OutData/casos_totales_rangos_edades.RData")

# 2. Casos Positivos ===========================================================

#    Este objeto contendra los casos unicamente positivos, de acuerdo a la
#    clasificacion reportada.

casos_positivos_re <- filter(casos_totales_re, CLASIFICACION_FINAL == 1 |
                                 CLASIFICACION_FINAL == 2 |
                                 CLASIFICACION_FINAL == 3 )

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_re, file = "03_Out/OutData/casos_positivos_rangos_edades.RData")
