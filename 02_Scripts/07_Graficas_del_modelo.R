# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)
library(ggmatplot)
library(plotly)
library(wesanderson)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")


# Se cargan los datos del modelo
source("02_Scripts/06_0_Resolucion_numerica_todos_los_grupos.R")


# Se cargan los datos de COVID
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")



# Gráfica de Infectados ====
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(4,12,20,28)],
        type = "l", xlab = "Tiempo", ylab = "Infectados", 
        main = "Infectados inferidos del modelo de COVID-19", 
        lty = 1, lwd = 2, col = viridis(4))
legend("topright", c("Infectados Grupo 1", "Infectados Grupo 2", 
                     "Infectados Grupo 3", "Infectados Grupo 4"),
                     fill = viridis (4))

grafica_infectados <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(4,12,20,28)],
                                plot_type = "line", color = wes_palette("FantasticFox1", 4), 
                                fill = wes_palette("FantasticFox1", 4),
                                linetype = 1, xlab = "Tiempo", ylab = "Población",
                                main = "Infectados inferidos del modelo heterogéneo",
                                legend_title = "Grupos", legend_label = c("Infectados Menores 18 años",
                                                                          "Infectados 18 - 39 años",
                                                                          "Infectados 40 - 59 años",
                                                                          "Infectados Mayores de 60 años"),
                                 lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


ggsave("03_Out/Plots/grafica_infectados_inferidos.jpeg", 
       plot = grafica_infectados, 
       width = 2487, height = 1791,units = "px")



# Gráfica de Recuperados ====
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(9,17,25,33)],
        type = "l", xlab = "Tiempo", ylab = "Recuperados", 
        main = "Recuperados inferidos del modelo de COVID-19", 
        lty = 1, lwd = 2, col = viridis(4))
legend("topright", c("Recuperados Grupo 1", "Recuperados Grupo 2", 
                     "Recuperados Grupo 3", "Recuperados Grupo 4"),
       fill = viridis (4))


grafica_recuperados <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(9,17,25,33)],
          plot_type = "line", color = wes_palette("FantasticFox1", 4), 
          fill = wes_palette("FantasticFox1", 4),
          linetype = 1, xlab = "Tiempo", ylab = "Población", 
          main = "Recuperados inferidos del modelo heterogéneo",
          legend_title = "Grupos", legend_label = c("Recuperados Menores 18 años",
                                                    "Recuperados 18 - 39 años",
                                                    "Recuperados 40 - 59 años",
                                                    "Recuperados Mayores de 60 años"),
          lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


ggsave("03_Out/Plots/grafica_recuperados_inferidos.jpeg", 
       plot = grafica_recuperados, 
       width = 2487, height = 1791, units = "px")


# Gráfica de Muertos ====
# Esta gráfica se obtiene a partir de los datos del modelo
matplot(out_all_groups[,1], out_all_groups[,c(8,16,24,32)],
        type = "l", xlab = "Tiempo", ylab = "Muertos", 
        main = "Muertos inferidos del modelo de COVID-19", 
        lty = 1, lwd = 2, col = viridis(4))
legend("topright", c("Muertos Grupo 1", "Muertos Grupo 2", 
                     "Muertos Grupo 3", "Muertos Grupo 4"),
       fill = viridis (4))

grafica_muertos <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(8,16,24,32)],
                             plot_type = "line", color = wes_palette("FantasticFox1", 4), 
                             fill = wes_palette("FantasticFox1", 4),
                             linetype = 1, xlab = "Tiempo", ylab = "Población",
                             main = "Muertos inferidos del modelo heterogéneo",
                             legend_title = "Grupos", legend_label = c("Muertos Menores 18 años",
                                                                       "Muertos 18 - 39 años",
                                                                       "Muertos 40 - 59 años",
                                                                       "Muertos Mayores de 60 años"),
                             lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


ggsave("03_Out/Plots/grafica_muertos_inferidos.jpeg", 
       plot = grafica_muertos, 
       width = 2487, height = 1791, units = "px")


# Grafica de Infectados, Recuperados y Muertos totales inferidos ====
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

plot_irm <- ggmatplot(x = inferidos_totales[,1], 
                      y = inferidos_totales[,c(34,35,36)],
                      plot_type = "line", color = wes_palette("Zissou1", 32, type =  "discrete"),
                      linetype = 1, main = "Infectados, Recuperados y Muertos inferidos por el modelo",
                      xlab = "Tiempo", ylab = "Población", 
                      legend_label = c("Infectados", "Recuperados", "Muertos"),
                      legend_title = "Grupos") +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 1))
        
plot_irm



# Grafica de Infectados, Recuperados y Muertos observados ====
colnames(casos_positivos_x_dia_re) <- c("rango_de_edad","FECHA_SINTOMAS", "casos_totales")
ggmatplot(x = casos_positivos_x_dia_re[,2],
          y = casos_positivos_x_dia_re[,3], plot_type = "l")



# Condiciones para hacer zoom en los datos inferidos
t_1 <- seq (0, 10, by = 0.001)
out_all_groups_zoom <- as.data.frame(ode(y     = state, times = t_1,
                                         func  = modelo_covid_all_groups,
                                         parms = parameters))
# Grafica de Susceptibles Inferidos ====
t_2 <- seq (0, 0.1, by = 0.1)
out_all_groups_zoom_S <- as.data.frame(ode(y     = state, times = t_2,
                                         func  = modelo_covid_all_groups,
                                         parms = parameters))
matplot(out_all_groups_zoom_S[,1], out_all_groups_zoom_S[,c(2,10,18,26)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Susceptibles inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(1,9,17,25)])
legend("topright", c("Susceptibles Grupo 1",
                     "Susceptibles Grupo 2",
                     "Susceptibles Grupo 3",
                     "Susceptibles Grupo 4"), 
       col = colores[c(1,9,17,25)], cex = 0.7, fill = colores[c(1,9,17,25)])



# Grafica de Expuestos Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(4,12,20,28)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Expuestos inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(2,10,18,26)])
legend("topright", c("Expuestos Grupo 1",
                     "Expuestos Grupo 2",
                     "Expuestos Grupo 3",
                     "Expuestos Grupo 4"), 
       col = colores[c(2,10,18,26)], cex = 0.7, fill = colores[c(2,10,18,26)])



# Grafica de Infectados Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(3,11,19,27)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Infectados inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(3,11,19,27)])
legend("topright", c("Infectados Grupo 1",
                     "Infectados Grupo 2",
                     "Infectados Grupo 3",
                     "Infectados Grupo 4"), 
       col = colores[c(3,11,19,27)], cex = 0.7, fill = colores[c(3,11,19,27)])



# Grafica de Infectados Leves Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(5,13,21,29)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Infectados Leves inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(4,12,20,28)])
legend("topright", c("Infectados Leves Grupo 1",
                     "Infectados Leves Grupo 2",
                     "Infectados Leves Grupo 3",
                     "Infectados Leves Grupo 4"), 
       col = colores[c(4,12,20,28)], cex = 0.7, fill = colores[c(4,12,20,28)])



# Grafica de Infectados Hospitalizados Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(6,14,22,30)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Infectados Hospitalizados inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(5,13,21,29)])
legend("topright", c("Infectados Hospitalizados Grupo 1",
                     "Infectados Hospitalizados Grupo 2",
                     "Infectados Hospitalizados Grupo 3",
                     "Infectados Hospitalizados Grupo 4"), 
       col = colores[c(5,13,21,29)], cex = 0.7, fill = colores[c(5,13,21,29)])



# Grafica de Infectados Intubados Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(7,15,23,31)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Infectados Intubados inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(6,14,22,30)])
legend("topright", c("Infectados Intubados Grupo 1",
                     "Infectados Intubados Grupo 2",
                     "Infectados Intubados Grupo 3",
                     "Infectados Intubados Grupo 4"), 
       col = colores[c(6,14,22,30)], cex = 0.7, fill = colores[c(6,14,22,30)])



# Grafica de Muertos Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(8,16,24,32)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Muertos inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(7,15,23,31)])
legend("topright", c("Muertos Grupo 1",
                     "Muertos Grupo 2",
                     "Muertos Grupo 3",
                     "Muertos Grupo 4"), 
       col = colores[c(7,15,23,31)], cex = 0.7, fill = colores[c(7,15,23,31)])



# Grafica de Recuperados Inferidos ====
matplot(out_all_groups_zoom[,1], out_all_groups_zoom[,c(9,17,25,33)],
        type = "l", xlab = "Tiempo", ylab = "Población", 
        main = "Recuperados inferidos del modelo", 
        lty = 1, lwd = 2, col = colores[c(8,16,24,32)])
legend("topright", c("Recuperados Grupo 1",
                     "Recuperados Grupo 2",
                     "Recuperados Grupo 3",
                     "Recuperados Grupo 4"), 
       col = colores[c(8,16,24,32)], cex = 0.7, fill = colores[c(8,16,24,32)])
