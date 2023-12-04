# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(lubridate)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se cargan los scripts del modelo
source("02_Scripts/05_Resolucion_numerica.R")

# Se cargan los datos del modelo
load("03_Out/OutData/casos_totales_rangos_edades.RData")
load("03_Out/OutData/casos_datos_x_grupos.RData")
load("03_Out/OutData/muertes_datos_x_grupos.RData")

# Probabilidades reales ========================================================
totales_reales <- mutate(casos_totales_re, individuo = 1)
## INFECTADOS ------------------------------------------------------------------
infectados_reales <- filter(totales_reales, CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 |
                         CLASIFICACION_FINAL == 3)
p_infec_real <-  sum(infectados_reales$individuo)/sum(totales_reales$individuo)

## HOSPITALIZADOS --------------------------------------------------------------
hospitalizados_reales <- filter(totales_reales, TIPO_PACIENTE == 2)
p_hosp_real <- sum(hospitalizados_reales$individuo)/sum(totales_reales$individuo)

## INTUBADOS
intubados_reales <- filter(totales_reales, INTUBADO == 1)
p_int_real <- sum(intubados_reales$individuo)/sum(totales_reales$individuo)

## MUERTES ---------------------------------------------------------------------
muertes_reales <- mutate(totales_reales, muerte = c
                         ( ifelse( !is.na( totales_reales$FECHA_DEF ), 
                                   "Muerte", "No muerte") ))
muertes_reales <- filter(muertes_reales, muerte == "Muerte")
p_muerte_real <- sum(muertes_reales$individuo)/sum(totales_reales$individuo)

## TABLA -----------------------------------------------------------------------
porcentajes_reales <- matrix(c(p_infec_real, p_hosp_real, p_int_real, p_muerte_real), 
                             nrow = 4)
rownames(porcentajes_reales) <- c("Inf", "Hosp", "Int", "Muer")
colnames(porcentajes_reales) <- "p"
porcentajes_reales

# Probabilidades del modelo ====================================================
# Calcular la proporción de individuos para cada columna
proporcion_individuos <- beta_t_out[, -1] / rowSums(beta_t_out[, -1])

# Añadir la columna de tiempo
proporcion_individuos <- cbind(beta_t_out[, 1, drop = FALSE], proporcion_individuos)

# Imprimir las primeras filas del resultado
head(proporcion_individuos)
#INFECTADOS --------------------------------------------------------------------
# Seleccionar las columnas I1, I2, I3, I4 y la columna de tiempo
subset_datos_I <- proporcion_individuos[c("time", "I1", "I2", "I3", "I4")]

# Sumar las columnas I1, I2, I3, I4
subset_datos_I$sum_I <- rowSums(subset_datos_I[, c("I1", "I2", "I3", "I4")])

# Filtrar el renglón 398
I_398 <- subset_datos_I[398, 6]
#HOSPITALIZADOS ----------------------------------------------------------------
# Seleccionar las columnas I_h1, I_h2, I_h3, I_h4 y la columna de tiempo
subset_datos_I_h <- proporcion_individuos[c("time", "I_h1", "I_h2", "I_h3", "I_h4")]

# Sumar las columnas I1, I2, I3, I4
subset_datos_I_h$sum_I_h <- rowSums(subset_datos_I_h[, c("I_h1", "I_h2", "I_h3", "I_h4")])

# Filtrar el renglón 398
I_h_398 <- subset_datos_I_h[398, 6]
#INTUBADOS ---------------------------------------------------------------------
# Seleccionar las columnas I_h1, I_h2, I_h3, I_h4 y la columna de tiempo
subset_datos_I_i <- proporcion_individuos[c("time", "I_i1", "I_i2", "I_i3", "I_i4")]

# Sumar las columnas I1, I2, I3, I4
subset_datos_I_i$sum_I_i <- rowSums(subset_datos_I_i[, c("I_i1", "I_i2", "I_i3", "I_i4")])

# Filtrar el renglón 398
I_i_398 <- subset_datos_I_i[398, 6]
#MUERTOS -----------------------------------------------------------------------
# Seleccionar las columnas I_h1, I_h2, I_h3, I_h4 y la columna de tiempo
subset_datos_M <- proporcion_individuos[c("time", "M1", "M2", "M3", "M4")]

# Sumar las columnas I1, I2, I3, I4
subset_datos_M$sum_M <- rowSums(subset_datos_M[, c("M1", "M2", "M3", "M4")])

# Filtrar el renglón 398
M_398 <- subset_datos_M[398, 6]

## TABLA -----------------------------------------------------------------------
pocentajes_modelo <- matrix(c(I_398,I_h_398,I_i_398,M_398), nrow = 4)
rownames(pocentajes_modelo) <- c("Inf", "Hosp", "Int", "Muer")
colnames(pocentajes_modelo) <- "p"
pocentajes_modelo

# datos ========================================================================
datos <- data.frame(
  estado = c("Inf", "Hosp", "Int", "Muer"),
  model = c(1.619934e-05, 2.119885e-06, 4.435176e-06, 1.017782e-03),
  real = c(0.47295361, 0.09022145, 0.01333372, 0.03219750)
)

# Graficas =====================================================================
plot_porcentajes <- ggplot(datos, aes(x = estado)) +
  geom_bar(aes(y = model, fill = "model"), stat = "identity", position = "stack") +
  geom_bar(aes(y = real, fill = "real"), stat = "identity", position = "stack") +
  labs(title = "Comparación entre porcentajes reales con el modelo",
       x = "Categoría",
       y = "Valor")
plot_porcentajes

# Proporciones de las edades en casos ==========================================
head(casos_x_grupos)
colores <- viridis(4)

plot_casos_observados <- ggplot(casos_x_grupos_corte,
                                aes(x = FECHA_SINTOMAS,
                                    y = casos_totales)) +
  geom_density(aes(fill = grupos,
                   col = grupos),
               position = "fill",
               stat = "identity") +
  labs(x = "Tiempo",
       y = "Densidad",
       title = "Casos Observados",
       fill = "Grupo") +
  scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                "18-39 años" = colores[2],
                                "40-59 años" = colores[3],
                                "Mayores de 60 años" = colores[4])) +
  scale_fill_manual(labels = c("Menores de 18 años" = "<18",
                               "18-39 años" = "18-39",
                               "40-59 años" = "40-59",
                               "Mayores de 60 años" = "60<"),
                    values = c("Menores de 18 años" = colores[1],
                               "18-39 años" = colores[2],
                               "40-59 años" = colores[3],
                               "Mayores de 60 años" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
  guides(color = "none") +
  theme_minimal() +
  theme(
    # title
    plot.title = element_text(size = 12, face = "bold"),
    # linea del eje
    axis.line = element_line(colour = "black", linewidth = 0.3),
    # eje x
    axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    # eje y
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    # leyenda
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
    )
plot_casos_observados

# jpeg("03_Out/Plots/proporcion_casos_observados.jpeg",
#      width = 5733, height = 4300, res = 800, units = "px")
# plot_casos_observados
# dev.off()

## Casos del modelo ------------------------------------------------------------
casos_modelo_1 <- data_frame(dias = beta_t_out_df$dias,
                           casos = c(beta_t_out_df$I1),
                           grupo = "<18")
casos_modelo_2 <- data_frame(dias = beta_t_out_df$dias,
                             casos = c(beta_t_out_df$I2),
                             grupo = "18-39")
casos_modelo_3 <- data_frame(dias = beta_t_out_df$dias,
                             casos = c(beta_t_out_df$I3),
                             grupo = "40-59")
casos_modelo_4 <- data_frame(dias = beta_t_out_df$dias,
                             casos = c(beta_t_out_df$I4),
                             grupo = "60<")
casos_modelo <- rbind(casos_modelo_1,
                      casos_modelo_2,
                      casos_modelo_3,
                      casos_modelo_4)

casos_modelo$grupo <- factor(casos_modelo$grupo,
                             levels = c("<18",
                                        "18-39",
                                        "40-59",
                                        "60<"))

plot_casos_modelo <- ggplot(casos_modelo,
                            aes(x = dias,
                                y = casos)) +
  geom_density(aes(fill = grupo,
                   col = grupo),
               position = "fill",
               stat = "identity") +
  labs(x = "Tiempo",
       y = "Densidad",
       title = "Casos modelo",
       fill = "Grupo") +
  scale_color_manual(values = c("<18" = colores[1],
                               "18-39" = colores[2],
                               "40-59" = colores[3],
                               "60<" = colores[4])) +
  scale_fill_manual(values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
  guides(color = "none") +
  theme_minimal() +
  theme(
    # title
    plot.title = element_text(size = 12, face = "bold"),
    # linea del eje
    axis.line = element_line(colour = "black", linewidth = 0.5),
    # eje x
    axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    # eje y
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    # leyenda
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
    )
plot_casos_modelo

# jpeg("03_Out/Plots/proporcion_casos_modelo.jpeg",
#      width = 5733, height = 4300, res = 800, units = "px")
# plot_casos_modelo
# dev.off()

# Muertes ----------------------------------------------------------------------

head(muertes_x_grupos)

plot_muertes_observadas <- ggplot(muertes_x_grupos_corte,
                                  aes(x = FECHA_DEF,
                                      y = casos)) +
  geom_density(aes(fill = grupos,
                   col = grupos),
               position = "fill",
               stat = "identity") +
  labs(x = "Tiempo",
       y = "Densidad",
       title = "Muertes Observadas",
       fill = "Grupo") +
  scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                "18-39 años" = colores[2],
                                "40-59 años" = colores[3],
                                "Mayores de 60 años" = colores[4])) +
  scale_fill_manual(labels = c("Menores de 18 años" = "<18",
                               "18-39 años" = "18-39",
                               "40-59 años" = "40-59",
                               "Mayores de 60 años" = "60<"),
                    values = c("Menores de 18 años" = colores[1],
                               "18-39 años" = colores[2],
                               "40-59 años" = colores[3],
                               "Mayores de 60 años" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
  guides(color = "none") +
  theme_minimal() +
  theme(
    # title
    plot.title = element_text(size = 12, face = "bold"),
    # linea del eje
    axis.line = element_line(colour = "black", linewidth = 0.3),
    # eje x
    axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    # eje y
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    # leyenda
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
  )
plot_muertes_observadas

# jpeg("03_Out/Plots/proporcion_muertes_observadas.jpeg",
#      width = 5733, height = 4300, res = 800, units = "px")
# plot_muertes_observadas
# dev.off()

## Muertes del modelo ----------------------------------------------------------

muertes_modelo_1 <- data_frame(dias = beta_t_out_df$dias,
                               casos = c(beta_t_out_df$M1),
                               grupo = "<18")
muertes_modelo_2 <- data_frame(dias = beta_t_out_df$dias,
                               casos = c(beta_t_out_df$M2),
                               grupo = "18-39")
muertes_modelo_3 <- data_frame(dias = beta_t_out_df$dias,
                               casos = c(beta_t_out_df$M3),
                               grupo = "40-59")
muertes_modelo_4 <- data_frame(dias = beta_t_out_df$dias,
                               casos = c(beta_t_out_df$M4),
                               grupo = "60<")
muertes_modelo <- rbind(muertes_modelo_1,
                        muertes_modelo_2,
                        muertes_modelo_3,
                        muertes_modelo_4)


muertes_modelo$grupo <- factor(muertes_modelo$grupo,
                             levels = c("<18",
                                        "18-39",
                                        "40-59",
                                        "60<"))

plot_muertes_modelo <- ggplot(muertes_modelo,
                              aes(x = dias,
                                  y = casos)) +
  geom_density(aes(fill = grupo,
                   col = grupo),
               position = "fill",
               stat = "identity") +
  labs(x = "Tiempo",
       y = "Densidad",
       title = "Muertes modelo",
       fill = "Grupo") +
  scale_color_manual(values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_fill_manual(values = c("<18" = colores[1],
                               "18-39" = colores[2],
                               "40-59" = colores[3],
                               "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
  guides(color = "none") +
  theme_minimal() +
  theme(
    # title
    plot.title = element_text(size = 12, face = "bold"),
    # linea del eje
    axis.line = element_line(colour = "black", linewidth = 0.5),
    # eje x
    axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    # eje y
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    # leyenda
    legend.position = "right",  # Posición de la leyenda
    legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10),  # Texto de la leyenda
    legend.spacing = unit(0.5, "cm")
  )
plot_muertes_modelo


# jpeg("03_Out/Plots/proporcion_muertes_modelo.jpeg",
#      width = 5733, height = 4300, res = 800, units = "px")
# plot_muertes_modelo
# dev.off()