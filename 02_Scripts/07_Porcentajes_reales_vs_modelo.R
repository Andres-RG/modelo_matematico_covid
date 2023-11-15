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
