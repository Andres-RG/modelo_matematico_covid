# Librerias necesarias

library(tidyr)
library(tidyverse)
library(deSolve)

head(beta_t_out)
head(casos_x_grupos_corte)
head(muertes_x_grupos_corte)

## Datos del modelo ============================================================
beta_t_out <- as.data.frame(ode(y = state,
                                times = times,
                                func  = beta_t_modelo_covid,
                                parms = parameters))
### Ajuste de las fechas del modelo --------------------------------------------
beta_t_out <- cbind.data.frame(days = seq(as.Date("2020/3/1"), as.Date("2021/4/2"),"days"),
                               beta_t_out)
beta_t_out <- as_tibble(beta_t_out)
### Casos Infectados del modelo ------------------------------------------------
prueba <- beta_t_out %>% select(c(days),I1,I2,I3,I4)
prueba2 <- data.frame(grupos = rep(c("I1","I2","I3","I4"), each=398),
                      FECHA_SINTOMAS = rep(prueba$days, 4),
                      casos_totales=c(prueba$I1,prueba$I2,prueba$I3,prueba$I4))
### Datos reales y del modelo --------------------------------------------------
concatenated_tibble <- bind_rows(casos_x_grupos_corte,prueba2)
### Ajuste de los datos ----- --------------------------------------------------
data_frame_filtrado <- concatenated_tibble %>%
  filter(FECHA_SINTOMAS <= as.Date("2021-04-07"))
data_frame <- data_frame_filtrado %>%
  mutate(tipo = ifelse(grupos %in% c("I1", "I2", "I3", "I4"), "Modelo", "Casos"))
casos_solo_filt <- filter(data_frame, tipo == "Casos")
modelo_solo_filt <- filter(data_frame, tipo == "Modelo")
### Grafica --------------------------------------------------------------------
colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")
#
modelo_vs_casos <- ggplot(data_frame_filtrado,
                          aes(x = FECHA_SINTOMAS)) +
  ###
  geom_line(data = modelo_solo_filt,
            aes(x = FECHA_SINTOMAS,
                y = casos_totales,
                color = grupos),
            lwd = 2) +
  ###
  geom_point(data = casos_solo_filt,
             aes(x = FECHA_SINTOMAS,
                 y = casos_totales,
                 color = grupos,
                 shape = grupos),
             size = 3) +
  ###
  labs(x = "Tiempo", 
       y = "No. Casos",
       color = "Grupos",
       title = "Modelo vs Casos observados") +
  ###
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  #
  scale_shape_manual(values = c(0,16,2,18), name = "Casos") +
  #
  scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                "18-39 años" = colores[2],
                                "40-59 años" = colores[3],
                                "Mayores de 60 años" = colores[4]),
                     name = "Casos") + 
  #
  scale_color_manual(values = c("I1" = colores[1],
                                "I2" = colores[2],
                                "I3" = colores[3],
                                "I4" = colores[4]),
                     name = "Modelo") +
  #
  scale_color_manual(breaks = c("Menores de 18 años",
                                "18-39 años",
                                "40-59 años",
                                "Mayores de 60 años",
                                "I1", "I2", "I3", "I4"),
                     values = c("Menores de 18 años" = colores[1],
                                "18-39 años" = colores[2],
                                "40-59 años" = colores[3],
                                "Mayores de 60 años" = colores[4],
                                "I1" = colores[1],
                                "I2" = colores[2],
                                "I3" = colores[3],
                                "I4" = colores[4])) +
  #
  guides(color = guide_legend(override.aes = list(shape = c(0,16,2,18))),
         shape = "none") +
  ###
  theme(panel.background = element_rect(),
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(colour = "black", size = 0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "right",  # Posición de la leyenda
        legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
        legend.text = element_text(size = 10),  # Texto de la leyenda
        legend.spacing = unit(0.5, "cm"))
modelo_vs_casos

jpeg("03_Out/Plots/modelo_vs_casos.jpeg",
     width = 5733, height = 4300, res = 500, units = "px")
modelo_vs_casos
dev.off()
