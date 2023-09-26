# Librerias necesarias

library(EpiEstim)
library(incidence)
library(ggplot2)

# Se cargan los datos
load("03_Out/OutData/conteo_casos_positivos_rango_edad.RData")
head(casos_positivos_re_conteo)

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# DATOS AL INICIO
# Visucalización de datos
incidencia <- casos_positivos_re_conteo[1:100, -3]
incidencia <- as.data.frame(incidencia)
colnames(incidencia) <- c("date", "I")
head(incidencia)
#jpeg("03_Out/Plots/incidencia_covid_qro.jpeg", width = 395, height = 285, res = 300, units = "mm")
plot(as.incidence(incidencia$I))
#dev.off()
# EpiEstim R0
res_parametric_si <- estimate_R(incidencia, 
                                method="parametric_si",
                                config = make_config(list(
                                    mean_si = 4.0, 
                                    std_si = 1.5))
)

head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)
#jpeg("03_Out/Plots/r0_epiestim.jpeg", width = 365, height = 265, res = 300, units = "mm")
#plot(res_parametric_si, legend = FALSE)
#dev.off()

# TODOS LOS DATOS
# Visucalización de datos
incidencia_all <- casos_positivos_re_conteo[, -3]
incidencia_all <- as.data.frame(incidencia_all)
colnames(incidencia_all) <- c("dates", "I")
head(incidencia_all)
#jpeg("03_Out/Plots/incidencia_covid_qro_all.jpeg", width = 395, height = 285, res = 300, units = "mm")
plot(as.incidence(incidencia_all$I, dates = incidencia_all$dates))
#dev.off()
# EpiEstim R0
res_parametric_si_all <- estimate_R(incid  = incidencia_all,
                                    method = "parametric_si",
                                    config = make_config(list(mean_si = 5.6,
                                                              std_si = 4.2))
                                    )

head(res_parametric_si_all$R)
plot(res_parametric_si_all, legend = FALSE)
#jpeg("03_Out/Plots/r0_epiestim_all.jpeg", width = 365, height = 265, res = 300, units = "mm")
plot(res_parametric_si_all, legend = FALSE)
#dev.off()
