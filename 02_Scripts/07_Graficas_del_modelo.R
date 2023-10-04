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
source("02_Scripts/06.5_Resolucion_numerica_modificacion_betas.R")

# Se cargan los datos de COVID
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")

colores <- c("#00BFFF", "#CD9B1D", "#7CCD7C", "#6A5ACD")

# Gráfica de Infectados ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_infectados <- ggmatplot(x = out[,1], 
                                out[,c(4,12,20,28)],
                                plot_type = "line", 
                                color = colores, 
                                fill = colores,
                                linetype = 1, 
                                xlab = "Tiempo", ylab = "Población",
                                legend_title = "Grupos", 
                                legend_label = c("Infectados Menores 18 años",
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
grafica_recuperados <- ggmatplot(x = out[,1], 
                                 out[,c(9,17,25,33)],
                                 plot_type = "line", 
                                 color = colores,
                                 fill = colores,
                                 linetype = 1, 
                                 xlab = "Tiempo", ylab = "Población",
                                 legend_title = "Grupos",
                                 legend_label = c("Recuperados Menores 18 años",
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
grafica_muertos <- ggmatplot(x = out[,1], 
                             y = out[,c(8,16,24,32)],
                             plot_type = "line", 
                             color = colores, 
                             fill = colores,
                             linetype = 1, 
                             xlab = "Tiempo", ylab = "Población",
                             legend_title = "Grupos", 
                             legend_label = c("Muertos Menores 18 años",
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
inferidos_totales <- mutate(out,
                            infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales <- mutate(inferidos_totales,
                            recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales <- mutate(inferidos_totales,
                            muertos_totales_inf = M1 + M2 + M3 + M4)


plot_irm <- ggmatplot(x = inferidos_totales[,1], 
                      y = inferidos_totales[,c(34,35,36)],
                      plot_type = "line",
                      linetype = 1, lwd = 1.5,
                      main = "Infectados, Recuperados y Muertos inferidos por el modelo",
                      xlab = "Tiempo", ylab = "Población", 
                      legend_label = c("Infectados", "Recuperados", "Muertos"),
                      legend_title = "Grupos") +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 1))
        
plot_irm
#ggsave("03_Out/Plots/grafica_inferidos_totales.jpeg", 
#       plot = plot_irm, 
#       width = 2487, height = 1791, units = "px")






# GRAFICAS DE MODELO CON BETAS MODIFICADAS
# Gráfica de Infectados ====
grafica_infectados_mod <- ggmatplot(x = out_betas[,1],
                                    y = out_betas[,c(4,12,20,28)],
                                    plot_type = "line",
                                    color = colores,
                                    fill = colores,
                                    linetype = 1,
                                    xlab = "Tiempo", ylab = "Población",
                                    legend_title = "Grupos",
                                    legend_label = c("Infectados Menores 18 años",
                                                     "Infectados 18 - 39 años",
                                                     "Infectados 40 - 59 años",
                                                     "Infectados Mayores de 60 años"),
                                    lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_infectados_inferidos_MODIFICADOS.jpeg", 
#       plot = grafica_infectados_mod, 
#       width = 2487, height = 1791,units = "px")



# Gráfica de Recuperados ====
grafica_recuperados_mod <- ggmatplot(x = out_betas[,1],
                                     y = out_betas[,c(9,17,25,33)],
                                     plot_type = "line",
                                     color = colores,
                                     fill = colores,
                                     linetype = 1,
                                     xlab = "Tiempo", ylab = "Población",
                                     legend_title = "Grupos",
                                     legend_label = c("Recuperados Menores 18 años",
                                                      "Recuperados 18 - 39 años",
                                                      "Recuperados 40 - 59 años",
                                                      "Recuperados Mayores de 60 años"),
                                     lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_recuperados_inferidos_MODIFICADOS.jpeg", 
#       plot = grafica_recuperados_mod, 
#       width = 2487, height = 1791, units = "px")


# Gráfica de Muertos ====
grafica_muertos_mod <- ggmatplot(x = out_betas[,1],
                                 y = out_betas[,c(8,16,24,32)],
                                 plot_type = "line",
                                 color = colores,
                                 fill = colores,
                                 linetype = 1,
                                 xlab = "Tiempo", ylab = "Población",
                                 legend_title = "Grupos",
                                 legend_label = c("Muertos Menores 18 años",
                                                  "Muertos 18 - 39 años",
                                                  "Muertos 40 - 59 años",
                                                  "Muertos Mayores de 60 años"),
                                 lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75))


#ggsave("03_Out/Plots/grafica_muertos_inferidos_MODIFICADOS.jpeg", 
#       plot = grafica_muertos_mod, 
#       width = 2487, height = 1791, units = "px")


# Grafica de Infectados, Recuperados y Muertos totales inferidos ====
inferidos_totales_mod <- mutate(out_betas,
                                infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales_mod <- mutate(inferidos_totales_mod,
                                recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales_mod <- mutate(inferidos_totales_mod,
                                muertos_totales_inf = M1 + M2 + M3 + M4)


plot_irm_mod <- ggmatplot(x = inferidos_totales_mod[,1],
                          y = inferidos_totales_mod[,c(34,35,36)],
                          plot_type = "line",
                          linetype = 1, lwd = 1.5,
                          main = "Infectados, Recuperados y Muertos inferidos por el modelo con BETAS modificadas",
                          xlab = "Tiempo", ylab = "Población",
                          legend_label = c("Infectados", "Recuperados", "Muertos"),
                          legend_title = "Grupos") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1))

plot_irm_mod
#ggsave("03_Out/Plots/grafica_inferidos_totales_MODIFICADOS.jpeg", 
#       plot = plot_irm_mod, 
#       width = 2487, height = 1791, units = "px")

