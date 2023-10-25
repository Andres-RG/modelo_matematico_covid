library(tidyr)
library(tidyverse)
library(deSolve)

head(beta_t_out)
head(casos_x_grupos_corte)
head(muertes_x_grupos_corte)

##---- CASOS
fecha_especifica <- as.Date("2021-04-07")
casos_x_grupos_corte_fecha <- casos_x_grupos_corte  %>%
    filter(FECHA_SINTOMAS <= fecha_especifica)

fechas_continuas <- seq(min(casos_x_grupos_corte_fecha$FECHA_SINTOMAS), 
                        max(casos_x_grupos_corte_fecha$FECHA_SINTOMAS), by = "1 day")
completo_casos_x_grupos_corte <- merge(
    data.frame(FECHA_SINTOMAS = fechas_continuas),
    casos_x_grupos_corte, 
    by = "FECHA_SINTOMAS", all.x = TRUE)

completo_casos_x_grupos_corte$casos_totales[is.na(completo_casos_x_grupos_corte$casos_totales)] <- 0

head(completo_casos_x_grupos_corte)

fechas <- unique(completo_casos_x_grupos_corte$FECHA_SINTOMAS)

fechas_beta_t_out <- mutate(beta_t_out, fecha_sintomas = fechas)



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


data_frame_filtrado <- concatenated_tibble %>%
    filter(FECHA_SINTOMAS <= as.Date("2021-04-07"))


casos_solo_filt <- data_frame_filtrado[1:1428,]
modelo_solo_filt <- data_frame_filtrado[1429:3020,]


ggplot(aes(x = FECHA_SINTOMAS)) +
    geom_line(data = casos_solo_filt,
              aes(y = casos_totales, color = grupos)) +
    geom_point(data = modelo_solo_filt,
               aes(y = casos_totales, color = grupos)) +
    labs(x = "Fecha_sintomas", y = "casos_totales", color = "Grupos") +
    theme_minimal()




combined_data <- rbind(casos_solo_filt, modelo_solo_filt)

# Crea el gráfico utilizando ggplot
ggplot(combined_data, 
       aes(x = FECHA_SINTOMAS, y = casos_totales)) +
    geom_line(data = modelo_solo_filt, 
              aes(x = FECHA_SINTOMAS, y = casos_totales, color = grupos)) +
    geom_point(data = casos_solo_filt, 
               aes(x = FECHA_SINTOMAS, y = casos_totales, color = grupos)) +
    labs(x = "Fecha de Síntomas", y = "Casos Totales", color = "Grupos")+ 
    theme_minimal()



