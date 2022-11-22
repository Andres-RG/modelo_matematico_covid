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


# Se cargan los datos del modelo
source("02_Scripts/06_0_Resolucion_numerica_todos_los_grupos.R")


# Se cargan los datos de COVID
load("03_Out/OutData/casos_totales_rangos_edades.RData")



# Gráfica de Infectados 
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(4,12,20,28)],
        type = "l", xlab = "Tiempo", ylab = "Infectados", 
        main = "Infectados inferidos del modelo de COVID-19", 
        lty = 1, lwd = 0.5, col = viridis(4))
legend("topright", c("Infectados Grupo 1", "Infectados Grupo 2", 
                     "Infectados Grupo 3", "Infectados Grupo 4"),
                     fill = viridis (4))

# Gráfica de Recuperados 
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(9,17,25,33)],
        type = "l", xlab = "Tiempo", ylab = "Recuperados", 
        main = "Recuperados inferidos del modelo de COVID-19", 
        lty = 1, lwd = 0.5, col = viridis(4))
legend("topright", c("Recuperados Grupo 1", "Recuperados Grupo 2", 
                     "Recuperados Grupo 3", "Recuperados Grupo 4"),
       fill = viridis (4))

# Gráfica de Muertos 
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(8,16,24,32)],
        type = "l", xlab = "Tiempo", ylab = "Muertos", 
        main = "Muertos inferidos del modelo de COVID-19", 
        lty = 1, lwd = 0.5, col = viridis(4))
legend("topright", c("Muertos Grupo 1", "Muertos Grupo 2", 
                     "Muertos Grupo 3", "Muertos Grupo 4"),
       fill = viridis (4))

# Grafica de Infectados, Recuperados y Muertos totales inferidos
inferidos_totales <- mutate(out_all_groups,
                            infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales <- mutate(inferidos_totales,
                            recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales <- mutate(inferidos_totales,
                            muertos_totales_inf = M1 + M2 + M3 + M4)
matplot(inferidos_totales[,1], inferidos_totales[,c(34, 35, 36)],
        col = viridis(3), type = "l", xlab = "Tiempo", ylab = "Población",
        main = "Infectados, Recuperados y Muertos totales inferidos del modelo",
        lty = 1, lwd = 0.5)
legend("topright", c("Infectados", "Recuperados", "Muertos"),
       fill = viridis (3))
