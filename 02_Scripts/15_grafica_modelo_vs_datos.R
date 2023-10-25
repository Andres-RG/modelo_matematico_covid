library(tidyr)
library(tidyverse)
library(deSolve)

head(beta_t_out)
head(casos_x_grupos_corte)
head(muertes_x_grupos_corte)

##---- CASOS
beta_t_out <-as.data.frame(ode(y = state,
                               times=times,
                               func  = beta_t_modelo_covid,
                               parms = parameters))
beta_t_out<-cbind.data.frame(days=seq(as.Date("2020/3/1"),as.Date("2021/4/2"),
                                      "days"),beta_t_out)
beta_t_out<-as_tibble(beta_t_out)
beta_t_out %>% select(c(days),I1,I2,I3,I4)-> prueba
prueba2<-data.frame(grupos=rep(c("I1","I2","I3","I4"),
                               each=398) ,
                    FECHA_SINTOMAS=rep(prueba$days,4),
                    casos_totales=c(prueba$I1,prueba$I2,prueba$I3,prueba$I4))
concatenated_tibble<-bind_rows(casos_x_grupos,prueba2)








# Cambiar la base de datos y las variables
beta_t_out <- as.data.frame(ode(y = state,
                                times = times,
                                func = beta_t_modelo_covid,
                                parms = parameters))
beta_t_out <- cbind.data.frame(days = seq(as.Date("2020/3/1"), as.Date("2021/4/2"), "days"), beta_t_out)
beta_t_out <- as_tibble(beta_t_out)
beta_t_out %>% select(c(days), M1, M2, M3, M4) -> prueba
prueba2 <- data.frame(grupos = rep(c("M1", "M2", "M3", "M4"), each = 398),
                      FECHA_SINTOMAS = rep(prueba$days, 4),
                      casos_totales = c(prueba$M1, prueba$M2, prueba$M3, prueba$M4))
concatenated_tibble <- bind_rows(muertes_x_grupos, prueba2)

data_frame_filtrado <- concatenated_tibble %>%
    filter(FECHA_SINTOMAS <= as.Date("2021-04-07"))

casos_solo_filt <- data_frame_filtrado[1:1428,]
modelo_solo_filt <- data_frame_filtrado[1429:3020,]

combined_data <- rbind(casos_solo_filt, modelo_solo_filt)

colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")
modelo_vs_casos <- ggplot(combined_data, 
                          aes(x = FECHA_SINTOMAS)) +
    geom_line(data = modelo_solo_filt, 
              aes(x = FECHA_SINTOMAS, y = casos_totales, color = grupos),
              size = 1) +
    geom_point(data = casos_solo_filt, 
               aes(x = FECHA_SINTOMAS, y = casos_totales, 
                   color = grupos, shape = grupos)) +
    labs(x = "Fecha de Síntomas", y = "Casos Totales", color = "Grupos", 
         title = "Modelo vs Casos observados", shape = "Grupos") +
    theme(axis.line = element_line(colour = "black", size = 0.6),
          plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, face = "bold")) +
    scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                  "18-39 años" = colores[2],
                                  "40-59 años" = colores[3],
                                  "Mayores de 60 años" = colores[4],
                                  "M1" = colores[1],
                                  "M2" = colores[2],
                                  "M3" = colores[3],
                                  "M4" = colores[4]))

data_frame_filtrado <- concatenated_tibble %>%
    filter(FECHA_SINTOMAS <= as.Date("2021-04-07"))


casos_solo_filt <- data_frame_filtrado[1:1428,]
modelo_solo_filt <- data_frame_filtrado[1429:3020,]


combined_data <- rbind(casos_solo_filt, modelo_solo_filt)


colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")
modelo_vs_casos <- ggplot(combined_data, 
       aes(x = FECHA_SINTOMAS)) +
    ###
    geom_line(data = modelo_solo_filt, 
              aes(x = FECHA_SINTOMAS, y = casos_totales, color = grupos),
              size = 1) +
    ###
    geom_point(data = casos_solo_filt, 
               aes(x = FECHA_SINTOMAS, y = casos_totales, 
                   color = grupos, shape = grupos)) +
    labs(x = "Fecha de Síntomas", y = "Casos Totales", color = "Grupos", 
         title = "Modelo vs Casos observados", shape = "Grupos") +
    theme(axis.line = element_line(colour = "black", size = 0.6),
          plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, face = "bold")) +
    scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                  "18-39 años" = colores[2],
                                  "40-59 años" = colores[3],
                                  "Mayores de 60 años" = colores[4],
                                  "I1" = colores[1],
                                  "I2" = colores[2],
                                  "I3" = colores[3],
                                  "I4" = colores[4]))
#ggsave("03_Out/Plots/modelo_vs_casos.jpeg", plot = modelo_vs_casos, 
#       width = 3187, height = 1791,units = "px")




##MUERTES------
beta_t_out <-as.data.frame(ode(y = state,
                               times=times,
                               func  = beta_t_modelo_covid,
                               parms = parameters))
beta_t_out<-cbind.data.frame(days=seq(as.Date("2020/3/28"),as.Date("2021/4/29"),
                                      "days"),beta_t_out)
beta_t_out<-as_tibble(beta_t_out)
beta_t_out %>% select(c(days),M1,M2,M3,M4)-> prueba
prueba2<-data.frame(grupos=rep(c("M1","M2","M3","M4"),each=398),
                    FECHA_MUERTE=rep(prueba$days,4),
                    muertes_totales=c(prueba$M1,prueba$M2,prueba$M3,prueba$M4))
concatenated_tibble<-bind_rows(muertes_x_grupos,prueba2)


data_frame_filtrado <- concatenated_tibble %>%
    filter(FECHA_MUERTE <= as.Date("2021-04-29"))


casos_solo_filt <- data_frame_filtrado[1:1428,]
modelo_solo_filt <- data_frame_filtrado[1429:3020,]


combined_data <- rbind(casos_solo_filt, modelo_solo_filt)


colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")
casos_solo_filt$color <- rep(colores[1:4], each = 357)  # Asigna los colores a "M1", "M2", "M3", "M4"
casos_solo_filt$shape <- rep(1:4, each = 357)  # Asigna las formas a "M1", "M2", "M3", "M4"

modelo_vs_muertes <- ggplot(combined_data, 
                          aes(x = FECHA_MUERTE)) +
    ###
    geom_line(data = modelo_solo_filt, 
              aes(x = FECHA_MUERTE, y = muertes_totales, color = grupos),
              size = 1) +
    ###
    geom_point(data = casos_solo_filt, 
               aes(x = FECHA_MUERTE, y = muertes_totales, 
                   color = factor(color), shape = factor(shape))) +
    labs(x = "Fecha Muerte", y = "Muertes Totales", color = "Grupos", 
         title = "Modelo vs Muertes observadas", shape = "Grupos") +
    theme(axis.line = element_line(colour = "black", size = 0.6),
          plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, face = "bold"))
#ggsave("03_Out/Plots/modelo_vs_casos.jpeg", plot = modelo_vs_casos, 
#       width = 3187, height = 1791,units = "px")









# Cambiar la base de datos y las variables
beta_t_out <- as.data.frame(ode(y = state,
                                times = times,
                                func = beta_t_modelo_covid,
                                parms = parameters))
beta_t_out <- cbind.data.frame(days = seq(as.Date("2020/3/1"), as.Date("2021/4/2"), "days"), beta_t_out)
beta_t_out <- as_tibble(beta_t_out)
beta_t_out %>% select(c(days), M1, M2, M3, M4) -> prueba
prueba2 <- data.frame(grupos = rep(c("M1", "M2", "M3", "M4"), each = 398),
                      FECHA_SINTOMAS = rep(prueba$days, 4),
                      casos_totales = c(prueba$M1, prueba$M2, prueba$M3, prueba$M4))
concatenated_tibble <- bind_rows(muertes_x_grupos, prueba2)

data_frame_filtrado <- concatenated_tibble %>%
    filter(FECHA_SINTOMAS <= as.Date("2021-04-07"))

casos_solo_filt <- data_frame_filtrado[1:1428,]
modelo_solo_filt <- data_frame_filtrado[1429:3020,]

combined_data <- rbind(casos_solo_filt, modelo_solo_filt)

colores <- c("#00BFFF", "#FFB90F", "#7CCD7C", "#6A5ACD")
modelo_vs_casos <- ggplot(combined_data, 
                          aes(x = FECHA_SINTOMAS)) +
    geom_line(data = modelo_solo_filt, 
              aes(x = FECHA_SINTOMAS, y = casos_totales, color = grupos),
              size = 1) +
    geom_point(data = casos_solo_filt, 
               aes(x = FECHA_SINTOMAS, y = casos_totales, 
                   color = grupos, shape = grupos)) +
    labs(x = "Fecha de Síntomas", y = "Casos Totales", color = "Grupos", 
         title = "Modelo vs Casos observados", shape = "Grupos") +
    theme(axis.line = element_line(colour = "black", size = 0.6),
          plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, face = "bold")) +
    scale_color_manual(values = c("Menores de 18 años" = colores[1],
                                  "18-39 años" = colores[2],
                                  "40-59 años" = colores[3],
                                  "Mayores de 60 años" = colores[4],
                                  "M1" = colores[1],
                                  "M2" = colores[2],
                                  "M3" = colores[3],
                                  "M4" = colores[4]))