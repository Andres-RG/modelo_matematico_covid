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
load("03_Out/OutData/conteo_casos_muerte.RData")
load("03_Out/OutData/datos_muerte.RData")
load("03_Out/OutData/muertes_datos_x_grupos.RData")
load("03_Out/OutData/casos_solo_fecha.RData")
load("03_Out/OutData/casos_positivos_re_m_vac.RData")
load("03_Out/OutData/casos_por_fecha_vac.RData")

# Grafica muertes vs recuperados Datos crudos ==================================
# Se genera una grafica donde se observan las fechas por meses, de los casos 
# positivos y por estructura de edad, si los pacientes fallecieron o no.
plot_positivos_muertes_y_no_raw <- ggplot(casos_por_fecha,
                                      aes(x = FECHA_SINTOMAS,
                                          group = interaction(rango_de_edad, 
                                                              muerte),
                                          col = muerte)) +
    geom_line(aes(y = NumCasos), size = 0.5, alpha = 0.75) +
    geom_smooth(aes(y = NumCasos, linetype = muerte), se = FALSE,
            size = 0.9, alpha = 1.5, show.legend = F) +
    facet_grid(rango_de_edad ~ ., scales = "free_y") +
    labs(title = "Comparación de casos de muertes vs recuperados de COVID-19",
         x = "Tiempo", y = "No. de casos", fill = "Muerte") +
    theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1), #texto en el eje x
        panel.grid.major = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.75), #Eje de la gráfica
        legend.position = "bottom",  # Posición de la leyenda
        legend.title = element_blank(),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm")
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + #agrega los meses
    scale_y_continuous(labels = scales::comma) + #agrega el numero de casos
    scale_fill_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                      labels = c("Fallecimientos", "Recuperados")) +
    scale_color_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                       labels = c("Fallecimientos", "Recuperados")) + 
    scale_linetype_manual(values = c("dashed", "solid"))

# El objeto se guarda como un objeto jpeg
plot_positivos_muertes_y_no_raw

# jpeg("03_Out/Plots/plot_casos_fallecidos_vs_recuperados_raw.jpeg",
#     width = 365, height = 265, res = 300, units = "mm")
# plot_positivos_muertes_y_no_raw
# dev.off()


# Grafica muertes vs recuperados Datos normalizados ============================
max_cases <- max(casos_por_fecha$NumCasos)
plot_positivos_muertes_y_no_nom <- ggplot(casos_por_fecha,
                                      aes(x = FECHA_SINTOMAS,
                                          group = interaction(rango_de_edad, muerte),
                                          col = muerte)) +
    geom_line(aes(y = NumCasos / max_cases), size = 0.5, alpha = 0.75) +
    geom_smooth(aes(y = NumCasos / max_cases, linetype = muerte), se = FALSE,
                size = 0.9, alpha = 1.5, show.legend = F) +
    facet_grid(rango_de_edad ~ ., scales = "free_y") +
    labs(title = "Comparación de casos de muertes vs recuperados con datos normalizados de COVID-19",
         x = "Tiempo", y = "No. de casos", fill = "Muerte") +
    theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.75),
        legend.position = "bottom",  # Posición de la leyenda
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm")
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                      labels = c("Fallecimientos", "Recuperados")) +
    scale_color_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                       labels = c("Fallecimientos", "Recuperados")) +
    scale_linetype_manual(values = c("dashed", "solid"))
# El objeto se guarda como un objeto jpeg
plot_positivos_muertes_y_no_nom

# jpeg("03_Out/Plots/plot_casos_fallecidos_vs_recuperados_nom.jpeg",
#     width = 365, height = 265, res = 300, units = "mm")
# plot_positivos_muertes_y_no_nom
# dev.off()

# Grafica vacunacion vs muertes ================================================
# Se va a generar una grafica donde se observen las muertes de todos los casos 
# positivos, separado por estructura de edad y donde se observa ademas la fecha
# correspondiente al esquema de vacunacion en curso.

#     Fechas de vacunacion, de acuerdo a la pag de la Secretaria de Salud
#     Diciembre 2020 - Febrero 2021 : Personal de salud === PRIMERA FASE
#     Febrero - Mayo 2021 : 60+                         === SEGUNDA FASE
#     Mayo - Junio 2021 : 50 - 59                       === TERCERA FASE
#     Junio - Julio 2021 : 40 - 49                      === CUARTA FASE
#     Julio 2021 - Marzo 2022 : resto                   === QUINTA FASE

fase_1 <- as.Date(c("2020-12-01", "2021-01-31"))
fase_2 <- as.Date(c("2021-02-01", "2021-04-30"))
fase_3 <- as.Date(c("2021-05-01", "2021-05-31"))
fase_4 <- as.Date(c("2021-06-01", "2021-06-30"))
fase_5 <- as.Date(c("2021-07-01", "2022-03-31"))

colores_fases <- c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcoral")

casos_por_fecha_vac$FECHA_SINTOMAS <- as.Date(casos_por_fecha_vac$FECHA_SINTOMAS)

plot_muertes_vac <- ggplot(casos_por_fecha_vac,
                           aes(x = FECHA_SINTOMAS,
                               group = interaction(rango_de_edad,
                                                   muerte),
                               col = muerte)) +
  geom_line(aes(y = NumCasos), size = 0.5, alpha = 0.75) +
  geom_smooth(aes(y = NumCasos, linetype = muerte), se = FALSE,
              size = 0.9, alpha = 1.5, show.legend = F) +
  facet_grid(rango_de_edad ~ ., scales = "free_y") +
  labs(title = "Comparación de casos de muertes vs recuperados de COVID-19",
       x = "Tiempo", y = "No. de casos", fill = "Muerte") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1), #texto en el eje x
    panel.grid.major = element_blank(),
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black", size = 0.75), #Eje de la gráfica
    legend.position = "bottom",  # Posición de la leyenda
    legend.title = element_blank(),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + #agrega los meses
  scale_y_continuous(labels = scales::comma) + #agrega el numero de casos
  scale_fill_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                    labels = c("Fallecimientos", "Recuperados")) +
  scale_color_manual(values = c("Muerte" = "red4", "No muerte" = "dodgerblue"),
                     labels = c("Fallecimientos", "Recuperados")) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_rect(data = data.frame(xmin = fase_1[1], xmax = fase_1[2]),
            aes(xmin = as.numeric(xmin) - 0.5, xmax = as.numeric(xmax) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = colores_fases[1], alpha = 0.5, inherit.aes = FALSE)

# El objeto se guarda como un objeto jpeg
plot_muertes_vac
