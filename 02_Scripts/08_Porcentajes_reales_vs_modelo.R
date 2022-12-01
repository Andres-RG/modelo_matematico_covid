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

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")


# Se cargan los scripts del modelo
source("02_Scripts/06_0_Resolucion_numerica_todos_los_grupos.R")

# Se cargan los datos del modelo
load("03_Out/OutData/casos_totales_rangos_edades.RData")




# Probabilidades reales ====
totales_reales <- mutate(casos_totales_re, individuo = 1)
## INFECTADOS
infectados_reales <- filter(totales_reales, CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 |
                         CLASIFICACION_FINAL == 3)
p_infec_real <-  sum(infectados_reales$individuo)/sum(totales_reales$individuo)

## HOSPITALIZADOS
hospitalizados_reales <- filter(totales_reales, TIPO_PACIENTE == 2)
p_hosp_real <- sum(hospitalizados_reales$individuo)/sum(totales_reales$individuo)

## INTUBADOS
intubados_reales <- filter(totales_reales, INTUBADO == 1)
p_int_real <- sum(intubados_reales$individuo)/sum(totales_reales$individuo)

## MUERTES
muertes_reales <- mutate(totales_reales, muerte = c
                         ( ifelse( !is.na( totales_reales$FECHA_DEF ), 
                                   "Muerte", "No muerte") ))
muertes_reales <- filter(muertes_reales, muerte == "Muerte")
p_muerte_real <- sum(muertes_reales$individuo)/sum(totales_reales$individuo)

## TABLA
porcentajes_reales <- c(p_infec_real, p_hosp_real, p_int_real, p_muerte_real)



# Probabilidades del modelo ====
## INFECCION
p_infect_modelo <- sum(c(out_all_groups[,4], out_all_groups[,12],
                         out_all_groups[,20], out_all_groups[,28]))/sum(
                             out_all_groups[,2:33]
                         )
## HOSPITALIZADOS
p_hosp_modelo <- sum(c(out_all_groups[,6], out_all_groups[,14],
                       out_all_groups[,22], out_all_groups[,30]))/sum(
                           out_all_groups[,2:33]
                       )
## INTUBADOS
p_intu_modelo <- sum(c(out_all_groups[,7], out_all_groups[,15],
                       out_all_groups[,23], out_all_groups[,31]))/sum(
                           out_all_groups[2:33]
                       )
## MUERTOS
p_muerte_modelo <- sum(c(out_all_groups[,8], out_all_groups[,16],
                         out_all_groups[,24], out_all_groups[,32]))/sum(
                             out_all_groups[2:33]
                         )
## TABLA
porcentajes_modelo <- c(p_infect_modelo, p_hosp_modelo,
                                   p_intu_modelo, p_muerte_modelo)

# TABLA
porcentajes <- data.frame(porcentajes_reales, porcentajes_modelo)
rownames(porcentajes) <- c("p_infeccion", "p_hospitalizacion",
                           "p_intubacion", "p_muerte")
porcentajes <- t(porcentajes)

