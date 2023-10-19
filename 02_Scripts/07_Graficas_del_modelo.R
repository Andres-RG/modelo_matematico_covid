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
source("02_Scripts/13_modelo_con_beta_t.R")

# Se cargan los datos de COVID
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")

colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")

# Gráfica de Infectados ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_infectados <- ggmatplot(x = out[,1], 
                                out[,c(4,12,20,28)],
                                plot_type = "line",
                                fill = colores,
                                linetype = 1, 
                                xlab = "Tiempo", ylab = "Población",
                                main = "Casos Infectados Inferidos",
                                legend_title = "Grupos", 
                                legend_label = c("Menores 18 años",
                                                 "18 - 39 años",
                                                 "40 - 59 años",
                                                 "Mayores de 60 años"),
                                lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 160000),  # Establece los límites
    breaks = seq(0, 160000, by = 40000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) +
  geom_hline(yintercept = seq(0, 160000, by = 40000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/grafica_infectados_inferidos.jpeg", 
#       plot = grafica_infectados, 
#       width = 2787, height = 1791,units = "px")



# Gráfica de Recuperados ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_recuperados <- ggmatplot(x = out[,1], 
                                 out[,c(9,17,25,33)],
                                 plot_type = "line",
                                 fill = colores,
                                 linetype = 1, 
                                 xlab = "Tiempo", ylab = "Población",
                                 main = "Casos Recuperados Inferidos",
                                 legend_title = "Grupos",
                                 legend_label = c("Menores 18 años",
                                                  "18 - 39 años",
                                                  "40 - 59 años",
                                                  "Mayores de 60 años"),
                                 lwd = 1) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 800000),  # Establece los límites
    breaks = seq(0, 800000, by = 100000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) +
  geom_hline(yintercept = seq(0, 800000, by = 100000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/grafica_recuperados_inferidos.jpeg", 
#       plot = grafica_recuperados,
#       width = 2787, height = 1791, units = "px")



# Gráfica de Muertos ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_muertos <- ggmatplot(x = out[,1], 
                             y = out[,c(8,16,24,32)],
                             plot_type = "line",
                             fill = colores,
                             linetype = 1, 
                             xlab = "Tiempo", ylab = "Población",
                             main = "Casos Fallecidos Inferidos",
                             legend_title = "Grupos", 
                             legend_label = c("Menores 18 años",
                                              "18 - 39 años",
                                              "40 - 59 años",
                                              "Mayores de 60 años"),
                             lwd = 1) +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 90000),  # Establece los límites
    breaks = seq(0, 90000, by = 10000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) +
  geom_hline(yintercept = seq(0, 90000, by = 10000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/grafica_muertos_inferidos.jpeg", 
#       plot = grafica_muertos, 
#       width = 2787, height = 1791, units = "px")


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
                      main = "Total de Casos Infectados, Recuperados y Muertos Inferidos",
                      xlab = "Tiempo", ylab = "Población", 
                      legend_label = c("Infectados", "Recuperados", "Fallecidos"),
                      legend_title = "Casos") +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(panel.background = element_rect(fill = "white"), 
              axis.line = element_line(colour = "black", size = 1)) +
  scale_y_continuous(
    limits = c(0, 2200000),  # Establece los límites
    breaks = seq(0, 2200000, by = 200000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 2200000, by = 200000), 
             linetype = "dashed", color = "gray")

        
#ggsave("03_Out/Plots/grafica_inferidos_totales.jpeg", 
#       plot = plot_irm, 
#       width = 2787, height = 1791, units = "px")








# GRAFICAS DE MODELO CON BETAS MODIFICADAS
# Gráfica de Infectados ====
grafica_infectados_mod <- ggmatplot(x = out_betas[,1],
                                    y = out_betas[,c(4,12,20,28)],
                                    plot_type = "line",
                                    color = colores,
                                    fill = colores,
                                    linetype = 1,
                                    xlab = "Tiempo", ylab = "Población",
                                    main = "Casos Infectados Inferidos con tasas de interacción",
                                    legend_title = "Grupos",
                                    legend_label = c("Menores 18 años",
                                                     "18 - 39 años",
                                                     "40 - 59 años",
                                                     "Mayores de 60 años"),
                                    lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 200000),  # Establece los límites
    breaks = seq(0, 200000, by = 20000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 200000, by = 20000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_grafica_infectados_inferidos.jpeg", 
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
                                     main = "Casos Recuperados Inferidos con tasas de interacción",
                                     legend_title = "Grupos",
                                     legend_label = c("Menores 18 años",
                                                      "18 - 39 años",
                                                      "40 - 59 años",
                                                      "Mayores de 60 años"),
                                     lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 800000),  # Establece los límites
    breaks = seq(0, 800000, by = 100000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 800000, by = 100000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_grafica_recuperados_inferidos.jpeg", 
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
                                 main = "Casos Fallecidos Inferidos con tasas de interacción",
                                 legend_title = "Grupos",
                                 legend_label = c("Menores 18 años",
                                                  "18 - 39 años",
                                                  "40 - 59 años",
                                                  "Mayores de 60 años"),
                                 lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 63000),  # Establece los límites
    breaks = seq(0, 63000, by = 10000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 63000, by = 10000), 
             linetype = "dashed", color = "gray")



#ggsave("03_Out/Plots/beta_modificada_grafica_muertos_inferidos.jpeg", 
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
                          main = "Casos Infectados, Recuperados y Fallecidos inferidos con tasas de interacción",
                          xlab = "Tiempo", ylab = "Población",
                          legend_label = c("Infectados", "Recuperados", "Fallecidos"),
                          legend_title = "Casos") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_y_continuous(
    limits = c(0, 2300000),  # Establece los límites
    breaks = seq(0, 2300000, by = 200000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 2300000, by = 200000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_grafica_inferidos_totales.jpeg", 
#       plot = plot_irm_mod, 
#       width = 2487, height = 1791, units = "px")

# Gráfica de Susceptibles ====
# Esta gráfica se obtiene a partir de los datos del modelo
grafica_susceptibles_mod <- ggmatplot(x = out_betas[,1],
                                      y = out_betas[,c(2,10,18,26)],
                                      plot_type = "line",
                                      color = colores,
                                      fill = colores,
                                      linetype = 1,
                                      xlab = "Tiempo", ylab = "Población",
                                      main = "Susceptibles",
                                      legend_title = "Grupos",
                                      legend_label = c("Susceptibles Menores 18 años",
                                                       "Susceptibles 18 - 39 años",
                                                       "Susceptibles 40 - 59 años",
                                                       "Susceptibles Mayores de 60 años"),
                                      lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75))












# GRAFICAS MODIFICADAS DATOS COREA
# Gráfica de Infectados ====
grafica_infectados_mod_v2 <- ggmatplot(x = out_betas_v2[,1],
                                       y = out_betas_v2[,c(4,12,20,28)],
                                       plot_type = "line",
                                       fill = colores,
                                       linetype = 1,
                                       xlab = "Tiempo", ylab = "Población",
                                       main = "Infectados con datos Corea",
                                       legend_title = "Grupos",
                                       legend_label = c("Menores 18 años",
                                                        "18 - 39 años",
                                                        "40 - 59 años",
                                                        "Mayores de 60 años"),
                                       lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 120000),  # Establece los límites
    breaks = seq(0, 120000, by = 20000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 120000, by = 20000), 
             linetype = "dashed", color = "gray")



#ggsave("03_Out/Plots/beta_modificada_corea_grafica_infectados_inferidos.jpeg", 
#       plot = grafica_infectados_mod_v2, 
#       width = 2487, height = 1791,units = "px")


# determinar porcentajes infectados
#time_i <- out_betas_v2[,1]
#I1 <- out_betas_v2[,4]
#I2 <- out_betas_v2[,12]
#I3 <- out_betas_v2[,20]
#I4 <- out_betas_v2[,28]
#dat_inf <- data.frame(time_i, I1, I2, I3, I4)
#head(dat_inf)


# grafica de porcentajes
#ggplot(dat_inf, aes(x = time_i)) +
#  geom_line(aes(y = I1), color = "#00BFFF", linetype = "solid", size = 1) +
#  geom_line(aes(y = I2), color = "#FFB90F", linetype = "dashed", size = 1) +
#  geom_line(aes(y = I3), color = "#7CCD7C", linetype = "dotted", size = 1) +
#  geom_line(aes(y = I4), color = "#6A5ACD", linetype = "dotdash", size = 1) +
#  labs(x = "Tiempo", y = "Porcentaje de casos", main = "P de casos infectados") +
#  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
#                     limits = c(0,1))



# Gráfica de Recuperados ====
grafica_recuperados_mod_v2 <- ggmatplot(x = out_betas_v2[,1],
                                        y = out_betas_v2[,c(9,17,25,33)],
                                        plot_type = "line",
                                        fill = colores,
                                        linetype = 1,
                                        xlab = "Tiempo", ylab = "Población",
                                        main = "Recuperados datos Corea",
                                        legend_title = "Grupos",
                                        legend_label = c("Menores 18 años",
                                                         "18 - 39 años",
                                                         "40 - 59 años",
                                                         "Mayores de 60 años"),
                                        lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 800000),  # Establece los límites
    breaks = seq(0, 800000, by = 100000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 800000, by = 100000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_corea_grafica_recuperados_inferidos.jpeg", 
#       plot = grafica_recuperados_mod_v2, 
#       width = 2487, height = 1791, units = "px")


# Gráfica de Muertos ====
grafica_muertos_mod_v2 <- ggmatplot(x = out_betas_v2[,1],
                                 y = out_betas_v2[,c(8,16,24,32)],
                                 plot_type = "line",
                                 fill = colores,
                                 linetype = 1,
                                 xlab = "Tiempo", ylab = "Población",
                                 main = "Fallecidos datos Corea",
                                 legend_title = "Grupos",
                                 legend_label = c("Muertos Menores 18 años",
                                                  "Muertos 18 - 39 años",
                                                  "Muertos 40 - 59 años",
                                                  "Muertos Mayores de 60 años"),
                                 lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 40000),  # Establece los límites
    breaks = seq(0, 40000, by = 5000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 40000, by = 5000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_corea_grafica_muertos_inferidos.jpeg", 
#       plot = grafica_muertos_mod_v2, 
#       width = 2487, height = 1791, units = "px")


# Grafica de Infectados, Recuperados y Muertos totales inferidos ====
inferidos_totales_mod_v2 <- mutate(out_betas_v2,
                                infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales_mod_v2 <- mutate(inferidos_totales_mod_v2,
                                recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales_mod_v2 <- mutate(inferidos_totales_mod_v2,
                                muertos_totales_inf = M1 + M2 + M3 + M4)


plot_irm_mod_v2 <- ggmatplot(x = inferidos_totales_mod_v2[,1],
                          y = inferidos_totales_mod_v2[,c(34,35,36)],
                          plot_type = "line",
                          linetype = 1, lwd = 1.5,
                          main = "Casos Infectados, Recuperados y Muertos con betas modificadas con datos Corea",
                          xlab = "Tiempo", ylab = "Población",
                          legend_label = c("Infectados", "Recuperados", "Muertos"),
                          legend_title = "Casos") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_y_continuous(
    limits = c(0, 2000000),  # Establece los límites
    breaks = seq(0, 2000000, by = 200000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) + 
  geom_hline(yintercept = seq(0, 2000000, by = 200000), 
             linetype = "dashed", color = "gray")


#ggsave("03_Out/Plots/beta_modificada_corea_grafica_inferidos_totales.jpeg", 
#       plot = plot_irm_mod_v2, 
#       width = 2487, height = 1791, units = "px")







## beta_t ----------------------
beta_t_grafica_infectados <- ggmatplot(x = beta_t_out[,1],
                                       y = beta_t_out[,c(4,12,20,28)],
                                       plot_type = "line",
                                       fill = colores,
                                       linetype = 1,
                                       xlab = "Tiempo", ylab = "Población",
                                       main = "Casos Infectados beta_t",
                                       legend_title = "Grupos",
                                       legend_label = c("Menores 18 años",
                                                        "18 - 39 años",
                                                        "40 - 59 años",
                                                        "Mayores de 60 años"),
                                       lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75))
beta_t_grafica_infectados




beta_t_grafica_recuperados <- ggmatplot(x = beta_t_out[,1], 
                                        y = beta_t_out[,c(9,17,25,33)],
                                        plot_type = "line",
                                        fill = colores,
                                        linetype = 1,
                                        xlab = "Tiempo", ylab = "Población",
                                        main = "Casos Recuperados beta_t",
                                        legend_title = "Grupos",
                                        legend_label = c("Menores 18 años",
                                                         "18 - 39 años",
                                                         "40 - 59 años",
                                                         "Mayores de 60 años"),
                                        lwd = 1) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 800000),  # Establece los límites
    breaks = seq(0, 800000, by = 100000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) +
  geom_hline(yintercept = seq(0, 800000, by = 100000), 
             linetype = "dashed", color = "gray")
beta_t_grafica_recuperados



beta_t_grafica_muertos <- ggmatplot(x = beta_t_out[,1], 
                             y = beta_t_out[,c(8,16,24,32)],
                             plot_type = "line",
                             fill = colores,
                             linetype = 1, 
                             xlab = "Tiempo", ylab = "Población",
                             main = "Casos Fallecidos beta_t",
                             legend_title = "Grupos", 
                             legend_label = c("Menores 18 años",
                                              "18 - 39 años",
                                              "40 - 59 años",
                                              "Mayores de 60 años"),
                             lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) +
  scale_y_continuous(
    limits = c(0, 75000),  # Establece los límites
    breaks = seq(0, 75000, by = 10000),  # Establece divisiones cada 100 unidades
    minor_breaks = NULL  # No se utilizan divisiones menores en este caso
  ) +
  geom_hline(yintercept = seq(0, 75000, by = 5000), 
             linetype = "dashed", color = "gray")
beta_t_grafica_muertos



beta_t_grafica_susceptibles <- ggmatplot(x = beta_t_out[,1], 
                                    y = beta_t_out[,c(2,10,18,26)],
                                    plot_type = "line",
                                    fill = colores,
                                    linetype = 1, 
                                    xlab = "Tiempo", ylab = "Población",
                                    main = "Casos Susceptibles beta_t",
                                    legend_title = "Grupos", 
                                    legend_label = c("Menores 18 años",
                                                     "18 - 39 años",
                                                     "40 - 59 años",
                                                     "Mayores de 60 años"),
                                    lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 0.75)) 
beta_t_grafica_susceptibles




beta_t_grafica_hospitalizados <- ggmatplot(x = beta_t_out[,1], 
                                         y = beta_t_out[,c(6,14,22,30)],
                                         plot_type = "line",
                                         fill = colores,
                                         linetype = 1, 
                                         xlab = "Tiempo", ylab = "Población",
                                         main = "Casos Hospitalizados beta_t",
                                         legend_title = "Grupos", 
                                         legend_label = c("Menores 18 años",
                                                          "18 - 39 años",
                                                          "40 - 59 años",
                                                          "Mayores de 60 años"),
                                         lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(colour = "black", size = 0.75)) 
beta_t_grafica_hospitalizados

