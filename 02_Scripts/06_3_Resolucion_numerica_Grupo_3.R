# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se carga la base de datos
load("03_Out/OutData/probabilidades_de_transicion.RData")
load("03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")


# Resolucion ====
## Funcion del modelo ====
modelo_covid_grupo_3 <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
        
        ## GRUPO 3
        dS3   <- - beta_3 * S3 * I3
        dE3   <- (beta_3 * S3 * I3) - ( alpha * E3 )
        dI3   <- ( alpha * E3 ) - ( ph_3 * delta_h * I3 ) - ( pl_3 * delta_l * I3 )
        dI_l3 <- ( pl_3 * delta_l * I3 ) - ( gamma_R * I_l3 )
        dI_h3 <- ( ph_3 * delta_h * I3 ) - ( pi_3 * delta_i * I_h3 ) - ( (1 - pi_3) * gamma_h * I_h3 )
        dI_i3 <- ( pi_3 * delta_i * I_h3 ) - ( mu_3 * delta_m * I_i3 ) - ( (1 - mu_3) * gamma_i * I_i3 )
        dM3   <-  mu_3 * delta_m * I_i3
        dR3   <- ( gamma_R * I_l3 ) + ( (1 - pi_3) * gamma_h * I_h3 ) + ( (1 - mu_3) * gamma_i * I_i3 )
        
        list(c(dS3, dE3, dI3, dI_l3, dI_h3, dI_i3, dM3, dR3))
        
    })
}

## Tiempo ====

t <- seq (0, 100, by = 0.01)

## Parametros ====

parameters <- c(
    
    beta_3  <- 0.5090940     ,
    
    alpha   <- 1/5.6         ,
    
    pl_3    <- 0.8420886     ,
    
    ph_3    <- 0.15791136    ,
    
    delta_l <- 1/5.5         ,
    
    delta_h <- 1/4           ,
    
    gamma_R <- 1/14          ,
    
    pi_3    <- 0.18827911    ,
    
    delta_i <- 1/1           ,
    
    gamma_h <- 1/12          ,
    
    mu_3    <- 0.8224699     ,
    
    delta_m <- 1/8           ,
    
    gamma_i <- 1/7           ,
    
    N       <- 539000
)

## Condiciones iniciales del sistema ====
# De acuerdo a la información obtenida del INEGI, la población del estado de 
# Querétaro es de 2,368,467 habitantes, al senso realizado en 2020. 
# Este valor de la poblacion se toma como el valor total de la población.
# Para el caso de cada estructura de edad definia, la población de individuos 
# suceptibles quedaria como:
#        Grupo 1: Menores de 18 años = 782000
#        Grupo 2: 18 - 39 años = 801000
#        Grupo 3: 40 - 59 años = 539000
#        Grupo 4: 60 - >70 años = 242000

state <- c(
    S3    = N - 1,
    E3    = 0,
    I3    = 1,
    I_l3  = 0,
    I_h3  = 0,
    I_i3  = 0,
    M3    = 0,
    R3    = 0
)

## Out ====
out <- as.data.frame(ode(y     = state, 
                         times = t, 
                         func   = modelo_covid_grupo_3,
                         parms = parameters))

## Grafica ====
pdf("03_Out/Plots/Modelo COVID del Grupo 3 para el Estado de Queretaro.pdf")
matplot(out[,1], out[,2:9], type ="l", xlab = "tiempo", ylab = "Población", 
        main = "Modelo COVID del Grupo 3 para el estado de Queretaro", 
        lwd = 2, lty = 1, col = viridis(8)) 

legend("right", c("Suceptibles"                             ,
                  "Expuestos"                               ,
                  "Infectados"                              ,
                  "Contagiados sintomáticos leves"          ,
                  "Contagiados sintomáticos Hospitalizados" ,
                  "Unidad de Terapia Intensiva"             ,
                  "Muertos"                                 ,
                  "Recuperados"                             )
       , col = viridis(8), fill = viridis(8), cex = 0.7)
dev.off()
