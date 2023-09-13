# Librerias necesarias

library(EpiEstim)
library(incidence)

# Se cargan los datos
load("03_Out/OutData/casos_solo_fecha.RData")
head(casos_por_fecha)

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# Visucalización de datos
incidencia <- casos_por_fecha[1:80, -c(2:3)]
incidencia <- as.data.frame(incidencia)
colnames(incidencia) <- c("date", "I")
head(incidencia)
plot(as.incidence(incidencia$I))

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