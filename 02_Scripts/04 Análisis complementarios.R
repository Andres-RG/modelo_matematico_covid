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
load("03_Out/OutData/casos_positivos_rangos_edades.RData")





# Tabla de casos positivos totales por dia ====

pos <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re$FECHA_SINTOMAS) ) {
    pos <- c(positivos,1) } # por cada uno de los positivos, 
                            # coloca un 1 en el vector-

casos_positivos_re_conteo <- mutate(casos_positivos_re, positivos = pos) 
# genera una nueva columna en la base de los casos_positivos_re
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos 
# los renglones. 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_re_conteo <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_re_conteo,
                                       FUN = sum)

# Genera otra columna en elobjeto para agregar el numero de dia
casos_positivos_re_conteo [,3] <- c(1:length(casos_positivos_re_conteo$FECHA_SINTOMAS))
colnames(casos_positivos_re_conteo)[3] <- "num.dia" 
casos_positivos_re_conteo

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_re_conteo, file = "03_Out/OutData/conteo_casos_positivos_rango_edad.RData")




# Grafica del total de casos positivos con tail probability ====
# Esta grafica contiene EL TOTAL de positivos por fecha de inciio de síntomas separado
# por rango de edades. Se guarda al mismo tiempo como un objeto png
png("03_Out/Plots/grafica de casos positivos tail probability.png", width = 550, height = 350)
ggplot(casos_positivos_re, aes(x = FECHA_SINTOMAS, y = rango_de_edad, fill = 0.5 - 
                                   abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T) +
    scale_fill_viridis_c(name = "Tail probability", direction = -1)
dev.off()


# Tabla de casos positivos por dia por rango de edad ====

# Primero se establece que cada renglon es equivalente a un individuo
casos_positivos_x_dia_re <- mutate(casos_positivos_re, individuo = 1)

# Con esta funcion, se suman todos los casos positivos por dia por rango de edad
casos_positivos_x_dia_re <- aggregate(casos_positivos_x_dia_re$individuo, 
                                      by = list(casos_positivos_x_dia_re$rango_de_edad,
                                                casos_positivos_x_dia_re$FECHA_SINTOMAS), 
                                      FUN = sum)
colnames(casos_positivos_x_dia_re) <- c("Rango de Edad", 
                                        "FECHA_SINTOMAS", 
                                        "Casos totales")

# Se obtiene una tabla con las personas positivas por dia y por rango de edad. No contempla 
# su estado (hospitalizados, intubados, etc)
casos_positivos_x_dia_re

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_x_dia_re, file = "03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")


