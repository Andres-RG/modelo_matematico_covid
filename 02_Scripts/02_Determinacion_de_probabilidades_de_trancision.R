# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se carga la base de datos
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_leves_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_hospitalizados_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_intubados_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_intubados_a_muerte_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_muerte_rangos_edades.RData")

# Las probabilidades de trancision se determinaran con la aplicacion de una 
# funcion, alojada en el script de funciones; esta funcion toma, de todos los 
# casos, los que cumplen con la condicion x, y los divide entre todos los casos
# que cumplen con la condicion y. Primero se debe colocar la base de datos 
# de la condicion a la cual va a transicionar, y la segunda base es de la 
# condicion de la cual proviene.

# Probabilidad de pasar de Suceptible a Infectado ==============================
# Toma todos los casos positivos y se dividen entre el total de todos los casos
# registrados, siendo positivos o negativos. 
ps_i <- probabilidades(casos_positivos_re, casos_totales_re)

# Probabilidad de pasar de Infecatdo a Leve (Caso Ambulatorio) =================
# Es necesario primero filtrar de todos los casos positivos, aquellos cuya 
# clasificacion es de tipo Ambulatorio. 
# Toma los casos positivos leves y se dividen entre el total de los casos 
# positivos
pi_l <- probabilidades(casos_positivos_leves_re, casos_positivos_re)

# Probabilidad de pasar de Infectado a Grave (Hospitalizado) ===================
# Es necesario primero filtrar de todos los casos positivos, aquellos cuya 
# clasificacion es de tipo Hospitalizado.
# Toma los casos positivos hospitalizados y se dividen entre el total de casos
# positivos
pi_h <- probabilidades(casos_positivos_hospitalizados_re, casos_positivos_re)

# Probabilidad de pasar de Grave a Intubado (ICU) ==============================
# Es necesario primero filtrar de todos los casos positivos y hospitalizados, 
# aquellos que fueron intubados, de acuerdo a la clasificacion establecida.
# Toma los casos positivos intubados y se dividen entre el total de casos 
# hospitalizados
ph_icu <- probabilidades(casos_positivos_intubados_re, casos_positivos_hospitalizados_re)

# Probabilidad de pasar de Intubado a Muerte ===================================
# Es necesario primer filtrar de todos los casos positivos e intubados, aquellos 
# que fallecieron
# Toma los casos positivos que fallecieron y se divide entre el total de casos 
# intubados
picu_m <- probabilidades(casos_positivos_muerte_re, casos_positivos_intubados_re)

probabilidades_de_transicion <- data.frame(ps_i,pi_l, pi_h, ph_icu, picu_m)
colnames(probabilidades_de_transicion) <- c("Susceptible --> Infectado",
                                            "Infectado --> Ambulatorio",
                                            "Infectado --> Grave",
                                            "Grave --> ICU",
                                            "ICU --> Muerte")
probabilidades_de_transicion
# Se guarda el objeto como un objeto .RData
# save(probabilidades_de_transicion, file = "03_Out/OutData/probabilidades_de_transicion.RData")
