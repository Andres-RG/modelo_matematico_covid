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


# Tabla de casos positivos totales por dia ====

pos <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re$FECHA_SINTOMAS) ) {
    pos <- c(pos, 1) } # por cada uno de los positivos, 
                       # coloca un 1 en el vector-

casos_positivos_re_conteo <- mutate(casos_positivos_re, positivos = pos) 
# genera una nueva columna en la base de los casos_positivos_re
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos 
# los renglones. 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_re_conteo <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_re_conteo,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_re_conteo [,3] <- c(1:length(casos_positivos_re_conteo$FECHA_SINTOMAS))
colnames(casos_positivos_re_conteo)[3] <- "num.dia" 
casos_positivos_re_conteo

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_re_conteo, file = "03_Out/OutData/conteo_casos_positivos_rango_edad.RData")




# Grafica del total de casos positivos con tail probability ====
# Esta grafica contiene EL TOTAL de positivos por fecha de inciio de síntomas separado
# por rango de edades. Se guarda al mismo tiempo como un objeto png
#png("03_Out/Plots/grafica de casos positivos tail probability.png", 
#    width = 550, height = 350)
plot_casos_positivos_tail_probability <- ggplot(casos_positivos_re, 
       aes(x = FECHA_SINTOMAS, y = rango_de_edad, 
           fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T) +
    scale_fill_viridis_c(name = "Tail probability", direction = -1)
plot_casos_positivos_tail_probability
#dev.off()

plot_total_casos_positivos_re <- ggplot(casos_positivos_re_conteo,
                                        aes(x = FECHA_SINTOMAS, y = positivos)) +
  geom_point()
plot_total_casos_positivos_re
#ggsave("03_Out/Plots/plot_casos_positivos_totales_conteo.jpeg", 
#       plot = plot_total_casos_positivos_re, width = 2887, height = 1464, units = "px")


# Tabla de casos positivos por dia por rango de edad ====

# Primero se establece que cada renglon es equivalente a un individuo
casos_positivos_x_dia_re <- mutate(casos_positivos_re, individuo = 1)

# Con esta funcion, se suman todos los casos positivos por dia por rango de edad
casos_positivos_x_dia_re <- aggregate(casos_positivos_x_dia_re$individuo, 
                                      by = list(casos_positivos_x_dia_re$rango_de_edad,
                                                casos_positivos_x_dia_re$FECHA_SINTOMAS), 
                                      FUN = sum)
colnames(casos_positivos_x_dia_re) <- c("rango_de_edad", 
                                        "FECHA_SINTOMAS", 
                                        "casos_totales")

# Se obtiene una tabla con las personas positivas por dia y por rango de edad. No contempla 
# su estado (hospitalizados, intubados, etc)
casos_positivos_x_dia_re

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_x_dia_re, file = "03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")

plot_pos_x_dia_re <- ggplot(casos_positivos_x_dia_re) +
  geom_point(mapping = aes(x = FECHA_SINTOMAS, y = casos_totales, color = rango_de_edad))
plot_pos_x_dia_re

#ggsave("03_Out/Plots/conteo_casos_totales_x_re.jpeg",
#       plot = plot_pos_x_dia_re, width = 2887, height = 1464, units = "px")

plot_pos_x_dia_re_2 <- ggplot(casos_positivos_x_dia_re, 
                              aes(x = FECHA_SINTOMAS, 
                                  y = casos_totales,
                                  color = rango_de_edad,
                                  shape = rango_de_edad)) + 
  geom_point(alpha = 0.4, size = 3) +
  geom_smooth(se = F, linetype = "dashed", size = 1.2, alpha = 1.5) +
  labs(title = "Casos positivos por día",
       x = "Fecha", y = "No. de casos",
       color = "Rangos de edad",   # Cambia el nombre de la leyenda de color
       shape = "Rangos de edad") +
  scale_shape_manual(values = seq(0, 25)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
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

#ggsave("03_Out/Plots/conteo_casos_totales_x_re_v2.jpeg",
#       plot = plot_pos_x_dia_re_2, width = 2887, height = 1464, units = "px")

# Definicion de parametros ====
## Parametros obtenidos por estructura de edad ====

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
# Se guarda el objeto como un .RData
# save(parms_estructura_edad, file = "03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")



# Grafica de casos positivos por rangos de edad. Geom_density ====

## Grafica 1
plot_positivos_re <- ggplot(casos_positivos_re, 
                            aes(x=FECHA_SINTOMAS, fill = rango_de_edad)) + 
  geom_density(position="stack") + 
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
ggsave("03_Out/Plots/plot_casos_positivos_geom_density_1.jpeg", 
       plot = plot_positivos_re, width = 2887, height = 1464, units = "px")

## Grafica 2
plot_positivos_re <- ggplot(casos_positivos_re, 
                            aes(x=FECHA_SINTOMAS, fill = rango_de_edad)) + 
  geom_density(position="fill") + 
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
ggsave("03_Out/Plots/plot_casos_positivos_geom_density_2.jpeg", 
       plot = plot_positivos_re, width = 2887, height = 1464, units = "px")

# GRAFICA CASOS POSITIVOS -----
plot_pos_re <- ggplot(casos_positivos_re, 
                            aes(x = FECHA_SINTOMAS, 
                                fill = rango_de_edad)) + 
  geom_bar(position="fill", stat="count") + 
  ggtitle("Casos positivos a COVID por rangos de edades 
          para el estado de Queretaro") + 
  labs(x = "Tiempo", y = "Casos") +
  labs(fill = "Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = T) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
plot_pos_re
jpeg("03_Out/Plots/plot_casos_positivos_v2.jpeg",
     width = 365, height = 265, res = 300, units = "mm")
plot_pos_re
dev.off()



casos <- casos_positivos_re_conteo[, -3]
plot_casos <- ggplot(casos, 
                     aes(x = FECHA_SINTOMAS,
                         y = positivos)) +
  geom_line(col = "#CD6839") +
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
#jpeg("03_Out/Plots/plot_casos_positivos_totales.jpeg",
#     width = 365, height = 265, res = 300, units = "mm")
plot_casos
#dev.off()

