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
#jpeg("03_Out/Plots/incidencia_covid_qro.jpeg", width = 395, height = 285, res = 300, units = "mm")
plot(as.incidence(incidencia$I, dates = incidencia$dates))
#dev.off()

# EpiEstim R0
res_parametric_si <- estimate_R(incid  = incidencia,
                                method = "parametric_si",
                                config = make_config(list(mean_si = 5.6,
                                                          std_si = 4.2))
                                )
head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)
#jpeg("03_Out/Plots/r0_epiestim.jpeg", width = 365, height = 265, res = 300, units = "mm")
plot(res_parametric_si, legend = FALSE)
#dev.off()

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







