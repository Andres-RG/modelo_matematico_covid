# Librerias necesarias

library(EpiEstim)
library(incidence)
library(ggplot2)

# Se cargan los datos
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
head(casos_positivos_re_conteo)

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# DATOS DE INCIDENCIA
incidencia <- casos_positivos_re_conteo[, -3]
incidencia <- as.data.frame(incidencia)
colnames(incidencia) <- c("dates", "I")
head(incidencia)
##complementar las fechas
fechas_continuas <- seq(min(incidencia$dates), 
                        max(incidencia$dates), by = "1 day")
incidencia <- merge(data.frame(dates = fechas_continuas),
                    incidencia, by = "dates", all.x = TRUE)
incidencia$I[is.na(incidencia$I)] <- 0
head(incidencia)

# GRAFICA DE INCIDENCIA
#
plot(as.incidence(incidencia$I, dates = incidencia$dates))
#
plot_incidencia <- ggplot(data = incidencia,
                          aes(x = dates,
                              y = I)) +
    geom_bar(stat = "identity", color = "gray26", fill = "gray26") +
    labs(x = "Tiempo",
         y = "Incidencia diaria",
         title = "Incidencia diaria de COVID-19 en el estado de Querétaro") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
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
plot_incidencia

# jpeg("03_Out/Plots/incidencia_covid_qro.jpeg",
#      width = 5733, height = 4300, res = 500, units = "px")
# plot_incidencia
# dev.off()

# EpiEstim R0
res_parametric_si <- estimate_R(incid  = incidencia,
                                method = "parametric_si",
                                config = make_config(list(mean_si = 5.6,
                                                          std_si = 4.2))
                                )
head(res_parametric_si$R)
#
plot(res_parametric_si, legend = FALSE)
#

# jpeg("03_Out/Plots/r0_epiestim.jpeg",
#      width = 5733, height = 4300, res = 500, units = "px")
# plot(res_parametric_si, legend = FALSE)
# dev.off()

# DATOS AL INICIO
# Visucalización de datos
incidencia_inicio <- incidencia[1:100,]
head(incidencia_inicio)
#jpeg("03_Out/Plots/incidencia_covid_qro_inicio.jpeg", width = 395, height = 285, res = 300, units = "mm")
plot(as.incidence(incidencia_inicio$I, dates = incidencia_inicio$dates))
#dev.off()
# EpiEstim R0
res_parametric_si_inicio <- estimate_R(incid  = incidencia_inicio,
                                       method = "parametric_si",
                                       config = make_config(list(mean_si = 5.6,
                                                                 std_si = 4.2))
                                       )
head(res_parametric_si_inicio$R)
plot(res_parametric_si_inicio, legend = FALSE)
#jpeg("03_Out/Plots/r0_epiestim_inicio.jpeg", width = 365, height = 265, res = 300, units = "mm")
plot(res_parametric_si_inicio, legend = FALSE)
#dev.off()







