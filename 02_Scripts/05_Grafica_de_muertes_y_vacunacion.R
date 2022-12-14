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




# De la base de datos de los casos positivos con rango de edad, se 
# genera otra columna donde se indica si el paciente fallecio o no. 
casos_solo_positivos_muerte_re <- mutate(casos_positivos_re, 
                                         Muerte = c ( ifelse( !is.na(casos_positivos_re$FECHA_DEF),
                                                              "Muerte", "No Muerte") ))
# La base de datos generada contiene solo los casos positivos, con la columna
# que indica si fallecio o no, y rangos de edades. Se guarada como un objeto 
# .RData
# save(casos_solo_positivos_muerte_re, 
#     file = "03_Out/OutData/casos_positivos_re_m.RData")

# Se genera una grafica donde se observan las fechas por meses, de los casos 
# positivos y por estructura de edad, si los pacientes fallecieron o no.
plot_positivos_muertes_y_no <- ggplot(casos_solo_positivos_muerte_re, 
       aes(x = FECHA_SINTOMAS,
           y = rango_de_edad,
           fill=Muerte)) +
    geom_density_ridges2(alpha = 0.5) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour = "black", size = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
# El objeto se guarda como un objeto png
png("03_Out/Plots/Grafica de muertes o no de casos positivos.png", width = 550, height = 350)
plot_positivos_muertes_y_no
dev.off()




# Se va a generar una grafica donde se observen las muertes de todos los casos 
# positivos, separado por estructura de edad y donde se observa ademas la fecha
# correspondiente al esquema de vacunacion en curso. 

# Fechas de vacunacion, de acuerdo a la pag de la Secretaria de Salud
# Diciembre 2020 - Febrero 2021 : Personal de salud === PRIMERA FASE
# Febrero - Mayo 2021 : 60+                         === SEGUNDA FASE
# Mayo - Junio 2021 : 50 - 59                       === TERCERA FASE
# Junio - Julio 2021 : 40 - 49                      === CUARTA FASE
# Julio 2021 - Marzo 2022 : resto                   === QUINTA FASE

# A la base de datos de casos_solo_positivos_muerte_re se le va a gregar la 
# columna de las fechas de vacunacion. Para esto, se usa la funcion 
# fechas_vacunacion, que agrega la etapa de vacunacion de acuerdo a la fecha 
# de inicio de sintomas.
vacunacion <- fechas_vacunacion(casos_solo_positivos_muerte_re$FECHA_SINTOMAS)
casos_solo_positivos_muerte_re <- mutate(casos_solo_positivos_muerte_re, 
                                         FECHAS_VACUNACION = vacunacion)
# Se guarda el nuevo objeto como un objeto .RData
# save(casos_solo_positivos_muerte_re, 
#     file = "03_Out/OutData/casos_positivos_re_m_vac.RData")
load("03_Out/OutData/casos_positivos_re_m_vac.RData")

plot_positivos_m_vacunacion <- ggplot(casos_solo_positivos_muerte_re, 
                                      aes(x = FECHA_SINTOMAS, 
                                          y = rango_de_edad, 
                                          col = Muerte, 
                                          fill = FECHAS_VACUNACION)) +
    geom_density_ridges2(alpha = 0.5, size = 1) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour = "black", size = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(y = "Rangos de edades", x = "Tiempo") +
    ggtitle("Densidad de casos positivos que murieron y que no muerieron con las fechas de vacunaci??n")
# Se guarda el objeto como un png
png("03_Out/Plots/Grafica de muertes de casos positivos x fechas de vacunacion.png", 
    width = 550, height = 350)
plot_positivos_m_vacunacion
dev.off()
