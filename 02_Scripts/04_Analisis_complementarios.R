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
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_solo_fecha.RData")
load("03_Out/OutData/muertes_x_grupos_corte.RData")
load("03_Out/OutData/casos_x_grupos_corte.RData")
load("03_Out/OutData/conteo_casos_muerte.RData")
load("03_Out/OutData/datos_muerte.RData")
load("03_Out/OutData/muertes_datos_x_grupos.RData")
load("03_Out/OutData/casos_positivos_re_m_vac.RData")
load("03_Out/OutData/casos_por_fecha_vac.RData")

# Definicion de parametros =====================================================
## Parametros obtenidos por estructura de edad

# ---------------- Grupo 1: Personas menores de 18 años
# ---------------- Grupo 2: Personas de 18 - 29 & 30 - 39
# ---------------- Grupo 3: Personas de 40 - 49 & 50 - 59
# ---------------- Grupo 4: Personas de 60 - 69 & Personas mayores de 70 años

### Suceptible a Infectado ( S -> I )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
SI_18_minor     <- probabilidades_de_transicion [ 1, 1 ]
SI_18_30        <- mean (probabilidades_de_transicion [ 2:3, 1])
SI_40_59        <- mean (probabilidades_de_transicion [ 4:5, 1])
SI_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 1])

### Infectado a Infectado Leve ( I -> L )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
IL_18_minor     <- probabilidades_de_transicion [ 1, 2 ]
IL_18_30        <- mean (probabilidades_de_transicion [ 2:3, 2])
IL_40_59        <- mean (probabilidades_de_transicion [ 4:5, 2])
IL_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 2])

### Infectado a Infectado Grave/Hospitalizado ( I -> G )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
IG_18_minor     <- probabilidades_de_transicion [ 1, 3 ]
IG_18_30        <- mean (probabilidades_de_transicion [ 2:3, 3])
IG_40_59        <- mean (probabilidades_de_transicion [ 4:5, 3])
IG_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 3])

### Infectado Grave/Hospitalizado a Intubado/ICU ( G -> ICU )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
GICU_18_minor     <- probabilidades_de_transicion [ 1, 4 ]
GICU_18_30        <- mean (probabilidades_de_transicion [ 2:3, 4])
GICU_40_59        <- mean (probabilidades_de_transicion [ 4:5, 4])
GICU_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 4])

### Intubado/ICU a Muerte ( ICU -> M )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
ICUM_18_minor     <- probabilidades_de_transicion [ 1, 5 ]
ICUM_18_30        <- mean (probabilidades_de_transicion [ 2:3, 5])
ICUM_40_59        <- mean (probabilidades_de_transicion [ 4:5, 5])
ICUM_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 5])

# Para poder visualizar estos resultados, se elabora una tabla
SI <- c(SI_18_minor, SI_18_30, SI_40_59, SI_60_70_higher)
IL <- c(IL_18_minor, IL_18_30, IL_40_59, IL_60_70_higher)
IG <- c(IG_18_minor, IG_18_30, IG_40_59, IG_60_70_higher)
GICU <- c(GICU_18_minor, GICU_18_30, GICU_40_59, GICU_60_70_higher)
ICUM <- c(ICUM_18_minor, ICUM_18_30, ICUM_40_59, ICUM_60_70_higher)

parms_estructura_edad <- cbind(SI, IL, IG, GICU, ICUM)
colnames(parms_estructura_edad) <- c("S -> I", "I -> L", "I -> H", "H -> ICU",
                                     "ICU -> M")
rownames(parms_estructura_edad) <- c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4")
parms_estructura_edad
# Se guarda el objeto como un .RData
# save(parms_estructura_edad, file = "03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")

# Grafica del total de casos positivos con tail probability ====================
# Esta grafica contiene EL TOTAL de positivos por fecha de inciio de síntomas 
# separado por rango de edades. Se guarda al mismo tiempo como un objeto png

plot_casos_positivos_tail_probability <- ggplot(casos_positivos_re, 
       aes(x = FECHA_SINTOMAS, y = rango_de_edad, 
           fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T) +
    scale_fill_viridis_c(name = "Tail probability", direction = -1)
plot_casos_positivos_tail_probability

# geom_pont total ==============================================================
plot_total_casos_positivos_re <- ggplot(casos_positivos_re_conteo,
                                        aes(x = FECHA_SINTOMAS,
                                            y = positivos)) +
  geom_point(col = "#8B0000",
             shape = 17) + 
  labs(x = "Tiempo",
       y = "Casos",
       title = "Casos positivos") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.line = element_line(colour = "black", size = 0.75),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"))
plot_total_casos_positivos_re

# jpeg("03_Out/Plots/plot_casos_positivos_totales_conteo.jpeg",
#      width = 5733, height = 4300, res = 500, units = "px")
# plot_total_casos_positivos_re
# dev.off()

# geom_pont por rango de edad ==================================================
plot_pos_x_dia_re_2 <- ggplot(casos_positivos_x_dia_re, 
                              aes(x = FECHA_SINTOMAS, 
                                  y = casos_totales,
                                  color = rango_de_edad,
                                  shape = rango_de_edad)) + 
  geom_point(alpha = 0.6,
             size = 2.5) +
  geom_smooth(se = F,
              linetype = "dashed",
              alpha = 0.2) +
  labs(title = "Casos positivos por día",
       x = "Fecha",
       y = "No. de casos",
       color = "Rangos de edad",   # Cambia el nombre de la leyenda de color
       shape = "Rangos de edad") +
  scale_shape_manual(values = seq(1,7)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.line = element_line(colour = "black", size = 0.75),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
  )

plot_pos_x_dia_re_2

# jpeg("03_Out/Plots/conteo_casos_totales_x_re_v2.jpeg",
#      width = 5733, height = 4300, res = 500, units = "px")
# plot_pos_x_dia_re_2
# dev.off()

# casos positivos totales acumulados ===========================================
casos <- casos_positivos_re_conteo[, -3]
plot_casos <- ggplot(casos, 
                     aes(x = FECHA_SINTOMAS,
                         y = positivos)) +
  geom_line(col = "#FF4500", size = 0.6) +
  ggtitle("Casos positivos totales") + 
  labs(x = "Tiempo", y = "Casos") +
  labs(fill = "Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = T) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
plot_casos

# ggsave("03_Out/Plots/plot_casos_positivos_totales.jpeg",
#        plot = plot_casos, width = 2887, height = 1864, units = "px")

colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")

# casos por grupos =============================================================

plot_casos_x_grupos <- ggplot(casos_x_grupos_corte, 
                              aes(x = FECHA_SINTOMAS)) + 
  geom_line(aes(y = casos_totales, color = grupos), size = 0.8) +
  labs(x = "Tiempo", y = "No. Casos", title = "Casos por grupos etarios",
       color = "Grupos") +
  theme(axis.text.x = element_text(size = 7,angle = 45, 
                                   hjust = 1, face = "bold"),
        panel.background = element_rect(fill = "gray87"),
        axis.line = element_line(colour = "black", size = 0.65),
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD"))
plot_casos_x_grupos

# jpeg("03_Out/Plots/casos_infectados_datos.jpg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_casos_x_grupos
# dev.off()

# muertes por grupos ===========================================================

plot_muertes_x_grupos <- ggplot(muertes_x_grupos_corte, 
                              aes(x = FECHA_DEF)) + 
  geom_line(aes(y = casos, color = grupos), size = 0.8) +
  labs(x = "Tiempo", y = "No. Muertes", title = "Muertes por grupos etarios",
       color = "Grupos") +
  theme(axis.text.x = element_text(size = 7,angle = 45,
                                   hjust = 1, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD"))
plot_muertes_x_grupos

# jpeg("03_Out/Plots/muertes_por_grupos.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_muertes_x_grupos
# dev.off()

# Grafica muertes vs recuperados Datos crudos ==================================
# Se genera una grafica donde se observan las fechas por meses, de los casos 
# positivos y por estructura de edad, si los pacientes fallecieron o no.
plot_positivos_muertes_y_no_raw <- ggplot(casos_por_fecha,
                                          aes(x = FECHA_SINTOMAS,
                                              group = interaction(rango_de_edad, 
                                                                  muerte),
                                              col = muerte)) +
  geom_line(aes(y = NumCasos), size = 0.5, alpha = 0.6) +
  geom_smooth(aes(y = NumCasos, linetype = "dashed"), se = FALSE,
              size = 0.9, alpha = 1.5, show.legend = F) +
  facet_grid(rango_de_edad ~ ., scales = "free_y") +
  labs(title = "Comparación de casos de muertes vs recuperados de COVID-19",
       x = "Tiempo", y = "No. de casos", fill = "Muerte") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), #texto en el eje x
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
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_positivos_muertes_y_no_raw
# dev.off()


# Grafica muertes vs recuperados Datos normalizados ============================
max_cases <- max(casos_por_fecha$NumCasos)
plot_positivos_muertes_y_no_nom <- ggplot(casos_por_fecha,
                                          aes(x = FECHA_SINTOMAS,
                                              group = interaction(rango_de_edad, muerte),
                                              col = muerte)) +
  geom_line(aes(y = NumCasos / max_cases), size = 0.5, alpha = 0.6) +
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
#     width = 5733, height = 4300, res = 500, units = "px")
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
fase_5 <- as.Date(c("2021-07-01", "2021-10-17"))

fase_1_coords <- data.frame(xmin = fase_1[1],
                            xmax = fase_1[2],
                            ymin = -Inf, ymax = Inf)
fase_2_coords <- data.frame(xmin = fase_2[1],
                            xmax = fase_2[2],
                            ymin = -Inf, ymax = Inf)
fase_3_coords <- data.frame(xmin = fase_3[1],
                            xmax = fase_3[2],
                            ymin = -Inf, ymax = Inf)
fase_4_coords <- data.frame(xmin = fase_4[1],
                            xmax = fase_4[2],
                            ymin = -Inf, ymax = Inf)
fase_5_coords <- data.frame(xmin = fase_5[1],
                            xmax = fase_5[2],
                            ymin = -Inf, ymax = Inf)

casos_por_fecha_vac$FECHA_SINTOMAS <- as.Date(casos_por_fecha_vac$FECHA_SINTOMAS)

plot_muertes_vac <- ggplot(casos_por_fecha_vac,
                           aes(x = FECHA_SINTOMAS,
                               group = interaction(rango_de_edad,
                                                   muerte),
                               col = muerte)) +
  geom_line(aes(y = NumCasos), size = 0.5, alpha = 0.6) +
  geom_smooth(aes(y = NumCasos, linetype = "dashed"), se = FALSE,
              size = 0.9, alpha = 1.5, show.legend = F) +
  facet_grid(rango_de_edad ~ ., scales = "free_y") +
  labs(title = "Comparación de casos de muertes y recuperados en fases de vacunación",
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
  scale_fill_manual(values = c("Muerte" = "firebrick3", "No muerte" = "deepskyblue"),
                    labels = c("Fallecimientos", "Recuperados")) +
  scale_color_manual(values = c("Muerte" = "firebrick3", "No muerte" = "deepskyblue"),
                     labels = c("Fallecimientos", "Recuperados")) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_rect(data = fase_1_coords,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "lightcoral", alpha = 0.5, inherit.aes = FALSE) + 
  geom_rect(data = fase_2_coords,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "tan1", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = fase_3_coords,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gold", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = fase_4_coords,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "lightgreen", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = fase_5_coords,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.5, inherit.aes = FALSE)

# El objeto se guarda como un objeto jpeg
plot_muertes_vac

# jpeg("03_Out/Plots/grafica_muertes_recuperados_etapas_vacunacion.jpeg",
#      width = 5733, height = 4300, res = 500, units = "px")
# plot_muertes_vac
# dev.off()
