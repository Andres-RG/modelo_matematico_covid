# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(lubridate)
library(ggmatplot)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se cargan los datos del modelo
source("02_Scripts/05_Resolucion_numerica.R")
source("02_Scripts/05.5_Resolucion_con_beta_t.R")

# Se cargan los datos de COVID
load("03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")
head(casos_positivos_x_dia_re)

colores <- viridis(4)
dates <- seq(as.Date("2020-03-01"),
             as.Date("2021-07-14"),
             by = 1)
out_df <- mutate(out, dias = dates)

# Graficas del modelo ==========================================================
## Gráfica de Infectados -------------------------------------------------------
grafica_infectados <- ggplot(out_df,
                             aes(x = dias)) +
  geom_line(aes(y = I1, color = "<18"), lwd = 2) +
  geom_line(aes(y = I2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = I3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = I4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Infectados del modelo",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59"= colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
grafica_infectados

# jpeg("03_Out/Plots/grafica_infectados_inferidos.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# grafica_infectados
# dev.off()

## Gráfica de Recuperados ------------------------------------------------------
grafica_recuperados <- ggplot(out_df,
                             aes(x = dias)) +
  geom_line(aes(y = R1, color = "<18"), lwd = 2) +
  geom_line(aes(y = R2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = R3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = R4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Recuperados del modelo",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
grafica_recuperados

# jpeg("03_Out/Plots/grafica_recuperados_inferidos.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# grafica_recuperados
# dev.off()

## Gráfica de Muertos ----------------------------------------------------------
grafica_muertos <- ggplot(out_df,
                              aes(x = dias)) +
  geom_line(aes(y = M1, color = "<18"), lwd = 2) +
  geom_line(aes(y = M2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = M3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = M4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Muertes del modelo",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
grafica_muertos

# jpeg("03_Out/Plots/grafica_muertos_inferidos.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# grafica_muertos
# dev.off()

## Gráfica de Hospitalizados ---------------------------------------------------
grafica_hospitalizados <- ggplot(out_df,
                                 aes(x = dias)) +
  geom_line(aes(y = I_h1, color = "<18"), lwd = 2) +
  geom_line(aes(y = I_h2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = I_h3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = I_h4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Hospitalizados del modelo",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59"= colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
grafica_hospitalizados

# jpeg("03_Out/Plots/grafica_hospitalizados_inferidos.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# grafica_hospitalizados
# dev.off()

## Grafica de Infectados, Recuperados y Muertos totales inferidos --------------
inferidos_totales <- mutate(out,
                            infectados_totales_inf = I1 + I2 + I3 + I4)
inferidos_totales <- mutate(inferidos_totales,
                            recuperados_totales_inf = R1 + R2 + R3 + R4)
inferidos_totales <- mutate(inferidos_totales,
                            muertos_totales_inf = M1 + M2 + M3 + M4)
inferidos_totales <- mutate(inferidos_totales,
                            hospitalizados_totales_inf = I_h1 + I_h2 + I_h3 + I_h4)

plot_irm <- ggmatplot(x = inferidos_totales[,1], 
                      y = inferidos_totales[,c(34,35,36,37)],
                      plot_type = "line",
                      linetype = 1,
                      lwd = 1.5,
                      main = "Total de Casos Infectados, Hospitalizados, Recuperados y Muertos Inferidos",
                      xlab = "Tiempo", ylab = "Población", 
                      legend_label = c("Infectados", "Recuperados", "Fallecidos", "Hospitalizados"),
                      legend_title = "Casos") +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
plot_irm
        
# jpeg("03_Out/Plots/grafica_inferidos_totales.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# plot_irm
# dev.off()

# Graficas del modelo con beta_t ===============================================
dates2 <- seq(as.Date("2020-03-01"),
             as.Date("2021-04-02"),
             by = 1)
beta_t_out_df <- mutate(beta_t_out, dias = dates2)
## Gráfica de Infectados -------------------------------------------------------
beta_t_infectados <- ggplot(beta_t_out_df,
                             aes(x = dias)) +
  geom_line(aes(y = I1, color = "<18"), lwd = 2) +
  geom_line(aes(y = I2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = I3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = I4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Infectados del modelo con beta_t",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
beta_t_infectados

# jpeg("03_Out/Plots/beta_t_grafica_infectados.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# beta_t_infectados
# dev.off()

## Gráfica de Recuperados ------------------------------------------------------
beta_t_recuperados <- ggplot(beta_t_out_df,
                              aes(x = dias)) +
  geom_line(aes(y = R1, color = "<18"), lwd = 2) +
  geom_line(aes(y = R2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = R3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = R4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Recuperados del modelo con beta_t",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
beta_t_recuperados

# jpeg("03_Out/Plots/beta_t_grafica_recuperados.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# beta_t_recuperados
# dev.off()

## Gráfica de Muertos ----------------------------------------------------------
beta_t_muertos <- ggplot(beta_t_out_df,
                          aes(x = dias)) +
  geom_line(aes(y = M1, color = "<18"), lwd = 2) +
  geom_line(aes(y = M2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = M3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = M4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Muertes del modelo con beta_t",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
beta_t_muertos

# jpeg("03_Out/Plots/beta_t_grafica_muertos.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# beta_t_muertos
# dev.off()

## Gráfica de Hospitalizados ---------------------------------------------------
beta_t_hospitalizados <- ggplot(beta_t_out_df,
                                aes(x = dias)) +
  geom_line(aes(y = I_h1, color = "<18"), lwd = 2) +
  geom_line(aes(y = I_h2, color = "18-39"), lwd = 2) +
  geom_line(aes(y = I_h3, color = "40-59"), lwd = 2) + 
  geom_line(aes(y = I_h4, color = "60<"), lwd = 2) +
  labs(x = "Tiempo",
       y = "Población",
       title = "Hospitalizados del modelo con beta_t",
       color = "Grupos") +
  scale_color_manual(breaks = c("<18","18-39","40-59","60<"),
                     values = c("<18" = colores[1],
                                "18-39" = colores[2],
                                "40-59" = colores[3],
                                "60<" = colores[4])) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + #agrega los meses
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
beta_t_hospitalizados

# jpeg("03_Out/Plots/beta_t_grafica_hospitalizados.jpeg",
#      width = 6333, height = 3700, res = 800, units = "px")
# beta_t_hospitalizados
# dev.off()

## Grafica de Infectados, Recuperados y Muertos totales inferidos --------------
beta_t_inferidos_totales <- mutate(beta_t_out_df,
                            infectados_totales_inf = I1 + I2 + I3 + I4)
beta_t_inferidos_totales <- mutate(beta_t_inferidos_totales,
                            recuperados_totales_inf = R1 + R2 + R3 + R4)
beta_t_inferidos_totales <- mutate(beta_t_inferidos_totales,
                            muertos_totales_inf = M1 + M2 + M3 + M4)
beta_t_inferidos_totales <- mutate(beta_t_inferidos_totales,
                                   hospitalizados_totales_inf = I_h1 + I_h2 + I_h3 + I_h4)

beta_t_plot_irm <- ggmatplot(x = beta_t_inferidos_totales[,1],
                             y = beta_t_inferidos_totales[,c(35,36,37,38)],
                             plot_type = "line",
                             linetype = 1, lwd = 1.5,
                             main = "Total de Casos Infectados, Hospitalizados, Recuperados y Muertos Inferidos",
                             xlab = "Tiempo", ylab = "Población",
                             legend_label = c("Infectados", "Recuperados", "Fallecidos", "Hospitalizados"),
                             legend_title = "Casos") +
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
beta_t_plot_irm

# jpeg("03_Out/Plots/beta_t_grafica_inferidos_totales.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# beta_t_plot_irm
# dev.off()

