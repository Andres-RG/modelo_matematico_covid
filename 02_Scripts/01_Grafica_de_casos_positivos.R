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
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")
load("03_Out/OutData/casos_solo_fecha.RData")

## STACK =======================================================================
plot_positivos_re <- ggplot(casos_positivos_re,
                            aes(x = FECHA_SINTOMAS,
                                col = rango_de_edad,
                                fill = rango_de_edad)) +
  geom_bar(position = "stack",
           stat = "count") +
  labs( x = "Tiempo",
        y = "Casos",
        title = "Casos positivos a COVID por rangos de edades para el estado de Queretaro",
        fill = "Rangos de edad") +
  scale_fill_viridis(discrete = T) +
  scale_color_manual(values = viridis(7)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  guides(color = F) +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_positivos_re

# jpeg("03_Out/Plots/casos_positivos_rango_edad.jpeg",
#     width = 5733, height = 4300, res = 700, units = "px")
# plot_positivos_re
# dev.off()

## DOTPLOT =====================================================================
plot_pos_dot <- ggplot(casos_positivos_x_dia_re,
                       aes(x = FECHA_SINTOMAS,
                           y = casos_totales,
                           col = rango_de_edad,
                           shape = rango_de_edad)) +
    geom_point(size = 1.5,
               stroke = 1,
               alpha = 0.9) +
    labs(x = "Tiempo",
         y = "Casos positivos",
         title = "Casos positivos a COVID-19",
         col = "Rango de edad",
         shape = "Rango de edad") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_shape_manual(values = c("18-" = 1,
                                  "18-29" = 2,
                                  "30-39" = 3,
                                  "40-49" = 4,
                                  "50-59" = 5,
                                  "60-69" = 6,
                                  "70+" = 7)) +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_pos_dot

# jpeg("03_Out/Plots/plot_casos_positivos_dotplot.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_pos_dot
# dev.off()

##FILL | PROPORCION ============================================================
plot_pos_fill <- ggplot(casos_positivos_re,
                        aes(x = FECHA_SINTOMAS,
                            col = rango_de_edad,
                            fill = rango_de_edad)) +
  geom_bar(position = "fill") +
  labs(x = "Tiempo",
       y = "Casos positivos",
       title = "Variación de proporción de casos positivos a COVID-19",
       fill = "Rango de edad") +
  scale_fill_manual(values = viridis(7)) +
  scale_color_manual(values = viridis(7)) +
  guides(color = F) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_pos_fill

# jpeg("03_Out/Plots/plot_casos_positivos_proporción.jpeg",
#     width = 5733, height = 4300, res = 700, units = "px")
# plot_pos_fill
# dev.off()

# Grafica de casos positivos por rangos de edad. Geom_density ==================
## Grafica 1 -------------------------------------------------------------------
plot_densidad_positivos_re_v1 <- ggplot(casos_positivos_re,
                                        aes(x = FECHA_SINTOMAS,
                                            fill = rango_de_edad)) +
  geom_density( position = "stack" ) +
  labs(x = "Tiempo",
       y = "Densidad",
       title = "Densidad de Casos positivos",
       fill = "Rangos de Edad") +
  scale_fill_viridis(discrete = T) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_densidad_positivos_re_v1

# jpeg("03_Out/Plots/plot_casos_positivos_geom_density_1.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_densidad_positivos_re_v1
# dev.off()

## Grafica 2 -------------------------------------------------------------------
plot_densidad_positivos_re_v2 <- ggplot(casos_positivos_re,
                                        aes( x = FECHA_SINTOMAS,
                                             fill = rango_de_edad)) +
  geom_density(position = "fill") +
  labs( x = "Tiempo",
        y = "Densidad",
        title = "Densidad de Casos positivos",
        fill = "Rangos de Edad") +
  scale_fill_viridis(discrete = T) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_densidad_positivos_re_v2

# jpeg("03_Out/Plots/plot_casos_positivos_geom_density_2.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_densidad_positivos_re_v2
# dev.off()
