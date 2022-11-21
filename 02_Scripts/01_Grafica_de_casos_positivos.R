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
load("01_RawData/datos_covid_qro.RData")

# De la base de datos inicial, se determinaran los rangos de edad de todos
# los casos positivos a Covid-19 hasta diciembre, 2021. A partir de esta 
# adicion de datos, se construye la grafica para casos positivos divididos
# por rangos de edades. 

# Este objeto contendra todos los casos, con la columna de rango de edades, y
# clasificados por estos mismos.
casos_totales_re <- mutate(datos_covid_qro, rango_de_edad = rangos_edades(datos_covid_qro$EDAD))

# Se guarda el objeto como un objeto .RData
# save(casos_totales_re, file = "03_Out/OutData/casos_totales_rangos_edades.RData")

# Este objeto contendra los casos unicamente positivos, de acuerdo a la clasificacion
# reportada.  
casos_positivos_re <- filter(casos_totales_re, CLASIFICACION_FINAL == 1 | 
                        CLASIFICACION_FINAL == 2 |
                        CLASIFICACION_FINAL == 3 )

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_re, file = "03_Out/OutData/casos_positivos_rangos_edades.RData")

# Se elabora la grafica de casos positivos separados por rango de edades
plot_positivos_re <- ggplot(casos_positivos_re, 
                            aes(x=FECHA_SINTOMAS, y=EDAD, fill = rango_de_edad)) + 
    geom_bar(position="stack", stat="identity") + 
    ggtitle("Casos positivos a COVID por rangos de edades 
          para el estado de Queretaro") + 
    labs(x="Tiempo", y="Casos") +
    labs(fill="Rangos de Edad") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour = "black", size = 1)) +
    scale_fill_viridis(discrete = T)

# El objeto que contiene la grafica se guarda como una imagen png
pdf("03_Out/Plots/Casos positivos a COVID por rangos de edades para el estado de Queretaro.pdf",
    paper = "a4r", width = 12, height = 9)
plot_positivos_re
dev.off()
