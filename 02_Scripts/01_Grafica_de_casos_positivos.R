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
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_positivos_rangos_edades.RData")
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")
load("03_Out/OutData/casos_solo_fecha.RData")

# De la base de datos inicial, se determinaran los rangos de edad de todos
# los casos positivos a Covid-19 hasta 17 de octubre, 2021. A partir de esta 
# adicion de datos, se construye la grafica para casos positivos divididos
# por rangos de edades.

# Procesamiento Inicial de Datos:-----------
# Este objeto contendra todos los casos, con la columna de rango de edades, y
# clasificados por estos mismos. Se hace uso de una función desarrollada.
# casos_totales_re <- mutate(datos_covid_qro, rango_de_edad = rangos_edades(datos_covid_qro$EDAD))

# Se guarda el objeto como un objeto .RData
# save(casos_totales_re, file = "03_Out/OutData/casos_totales_rangos_edades.RData")

# Este objeto contendra los casos unicamente positivos, de acuerdo a la clasificacion
# reportada.
casos_positivos_re <- filter(casos_totales_re, CLASIFICACION_FINAL == 1 |
                                 CLASIFICACION_FINAL == 2 |
                                 CLASIFICACION_FINAL == 3 )

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_re, file = "03_Out/OutData/casos_positivos_rangos_edades.RData")

# GRAFICAS --------------
## STACK =====
plot_positivos_re <- ggplot(casos_positivos_re, 
                            aes(x=FECHA_SINTOMAS, fill = rango_de_edad)) + 
    geom_bar(position="stack", stat="count") + 
    ggtitle("Casos positivos a COVID por rangos de edades 
          para el estado de Queretaro") + 
    labs(x="Tiempo", y="Casos") +
    labs(fill="Rangos de Edad") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour = "black", size = 1)) +
    scale_fill_viridis(discrete = T)
plot_positivos_re
# El objeto que contiene la grafica se guarda como una imagen jpeg
#jpeg("03_Out/Plots/Casos positivos a COVID por rangos de edades para el estado de Queretaro.jpeg",
#     width = 365, height = 265, res = 300, units = "mm")
plot_positivos_re
#dev.off()

## DOTPLOT =====
plot_pos_dot <- ggplot(casos_positivos_x_dia_re,
                       aes(x = FECHA_SINTOMAS,
                           y = casos_totales,
                           col = rango_de_edad,
                           shape = rango_de_edad)) +
    geom_point(size = 1.5, stroke = 1) +
    labs(x = "Tiempo", y = "Casos positivos",
         title = "Casos positivos a COVID-19",
         col = "Rango de edad", shape = "Rango de edad") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_shape_manual(values = c("18-" = 1,
                                  "18-29" = 2,
                                  "30-39" = 3,
                                  "40-49" = 4,
                                  "50-59" = 5,
                                  "60-69" = 6,
                                  "70+" = 7)) +
    theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", size = 0.75),
        # leyenda
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"),
        # background
        panel.grid.major.y = element_line(color = "black", size = 0.5, 
                                          linetype = 2),  # Líneas horizontales al eje x
        panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula menores
        panel.background = element_blank(),
        panel.grid.major.x = element_blank()
    )

plot_pos_dot

#ggsave("03_Out/Plots/plot_casos_positivos_dotplot.jpeg",
#       plot = plot_pos_dot, width = 2287, height = 1464, units = "px")

##FILL | PROPORCION =======
plot_pos_fill <- ggplot(casos_positivos_re,
                        aes(x = FECHA_SINTOMAS,
                            fill = rango_de_edad)) +
    geom_bar(position = "fill") +
    labs(x = "Tiempo", y = "Casos positivos",
         title = "Variación de proporción de casos positivos a COVID-19",
         fill = "Rango de edad") +
    scale_fill_manual(values = viridis(7)) +
    theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", size = 0.75),
        # leyenda
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"),
        # background
        panel.grid.major.y = element_line(color = "black", size = 0.5, 
                                          linetype = 2),  # Líneas horizontales al eje x
        panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula menores
        panel.background = element_blank(),
        panel.grid.major.x = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")

plot_pos_fill

#ggsave("03_Out/Plots/plot_casos_positivos_proporción.jpeg",
#       plot = plot_pos_fill, width = 2287, height = 1464, units = "px")