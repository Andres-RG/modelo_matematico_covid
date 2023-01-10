# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)

# Se carga la base de datos
load("modelo_matematico_covid/03_Out/OutData/casos_positivos_re_m.RData")

#
plot_positivos_muertes_y_no <- ggplot(casos_solo_positivos_muerte_re, 
                                      aes(x = FECHA_SINTOMAS,
                                          y = rango_de_edad,
                                          fill= Muerte)) +
  geom_bar(stat = "identity", orientation = "x")
plot_positivos_muertes_y_no
