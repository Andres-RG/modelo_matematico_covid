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
source("02_Scripts/06_Resolucion_numerica.R")


# Se cargan los datos de COVID
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")



# Gráfica de Infectados ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_infectados <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(4,12,20,28)],
                                plot_type = "line", color = wes_palette("FantasticFox1", 4), 
                                fill = wes_palette("FantasticFox1", 4),
                                linetype = 1, xlab = "Tiempo", ylab = "Población",
                                legend_title = "Grupos", legend_label = c("Infectados Menores 18 años",
                                                                          "Infectados 18 - 39 años",
                                                                          "Infectados 40 - 59 años",
                                                                          "Infectados Mayores de 60 años"),
                                 lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_infectados_inferidos.jpeg", 
#       plot = grafica_infectados, 
#       width = 2487, height = 1791,units = "px")



# Gráfica de Recuperados ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_recuperados <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(9,17,25,33)],
          plot_type = "line", color = wes_palette("FantasticFox1", 4), 
          fill = wes_palette("FantasticFox1", 4),
          linetype = 1, xlab = "Tiempo", ylab = "Población",
          legend_title = "Grupos", legend_label = c("Recuperados Menores 18 años",
                                                    "Recuperados 18 - 39 años",
                                                    "Recuperados 40 - 59 años",
                                                    "Recuperados Mayores de 60 años"),
          lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_recuperados_inferidos.jpeg", 
#       plot = grafica_recuperados, 
#       width = 2487, height = 1791, units = "px")


# Gráfica de Muertos ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_muertos <- ggmatplot(x = out_all_groups[,1], out_all_groups[,c(8,16,24,32)],
                             plot_type = "line", color = wes_palette("FantasticFox1", 4), 
                             fill = wes_palette("FantasticFox1", 4),
                             linetype = 1, xlab = "Tiempo", ylab = "Población",
                             legend_title = "Grupos", legend_label = c("Muertos Menores 18 años",
                                                                       "Muertos 18 - 39 años",
                                                                       "Muertos 40 - 59 años",
                                                                       "Muertos Mayores de 60 años"),
                             lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_muertos_inferidos.jpeg", 
#       plot = grafica_muertos, 
#       width = 2487, height = 1791, units = "px")


# Grafica de Infectados, Recuperados y Muertos totales inferidos ====
inferidos_totales <- mutate(out_all_groups,
                            infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales <- mutate(inferidos_totales,
                            recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales <- mutate(inferidos_totales,
                            muertos_totales_inf = M1 + M2 + M3 + M4)


plot_irm <- ggmatplot(x = inferidos_totales[,1], 
                      y = inferidos_totales[,c(34,35,36)],
                      plot_type = "line", color = wes_palette("FantasticFox1", 3),
                      linetype = 1, lwd = 1.5,
                      main = "Infectados, Recuperados y Muertos inferidos por el modelo",
                      xlab = "Tiempo", ylab = "Población", 
                      legend_label = c("Infectados", "Recuperados", "Muertos"),
                      legend_title = "Grupos") +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 1))
        
plot_irm

