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
                                        aes(x = FECHA_SINTOMAS, y = positivos)) +
  geom_point()
plot_total_casos_positivos_re
#ggsave("03_Out/Plots/plot_casos_positivos_totales_conteo.jpeg", 
#       plot = plot_total_casos_positivos_re, width = 2887, height = 1464, units = "px")

# geom_pont por rango de edad ==================================================
plot_pos_x_dia_re_2 <- ggplot(casos_positivos_x_dia_re, 
                              aes(x = FECHA_SINTOMAS, 
                                  y = casos_totales,
                                  color = rango_de_edad,
                                  shape = rango_de_edad)) + 
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(se = F, linetype = "dashed", size = 1.2, alpha = 1.5) +
  labs(title = "Casos positivos por día",
       x = "Fecha",
       y = "No. de casos",
       color = "Rangos de edad",   # Cambia el nombre de la leyenda de color
       shape = "Rangos de edad") +
  scale_shape_manual(values = seq(0, 25)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", size = 0.75),
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
  )

plot_pos_x_dia_re_2

# ggsave("03_Out/Plots/conteo_casos_totales_x_re_v2.jpeg",
#        plot = plot_pos_x_dia_re_2, width = 2887, height = 1864, units = "px")

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
  theme(axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
        panel.background = element_rect(fill = "gray87"),
        axis.line = element_line(colour = "black", size = 0.65),
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD"))
plot_casos_x_grupos

# ggsave(filename = "03_Out/Plots/casos_infectados_datos.jpg",
#        plot = plot_casos_x_grupos,
#        width = 2487, height = 1791,
#        units = "px")

# muertes por grupos ===========================================================

plot_muertes_x_grupos <- ggplot(muertes_x_grupos_corte, 
                              aes(x = FECHA_DEF)) + 
  geom_line(aes(y = casos, color = grupos), size = 0.8) +
  labs(x = "Tiempo", y = "No. Muertes", title = "Muertes por grupos etarios",
       color = "Grupos") +
  theme(axis.text.x = element_text(size = 7,angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", size = 0.65),
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11, face = "bold")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD"))
plot_muertes_x_grupos

ggsave("03_Out/Plots/muertes_por_grupos.jpeg", 
       plot = plot_muertes_x_grupos,
       width = 2887, height = 1864,units = "px")

