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
modelo_covid_grupo_2 <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    
    ## GRUPO 2
    dS2   <- - beta_2 * S2 * I2
    dE2   <- (beta_2 * S2 * I2) - ( alpha * E2 )
    dI2   <- ( alpha * E2 ) - ( ph_2 * delta_h * I2 ) - ( pl_2 * delta_l * I2 )
    dI_l2 <- ( pl_2 * delta_l * I2 ) - ( gamma_R * I_l2 )
    dI_h2 <- ( ph_2 * delta_h * I2 ) - ( pi_2 * delta_i * I_h2 ) - ( (1 - pi_2) * gamma_h * I_h2 )
    dI_i2 <- ( pi_2 * delta_i * I_h2 ) - ( mu_2 * delta_m * I_i2 ) - ( (1 - mu_2) * gamma_i * I_i2 )
    dM2   <-  mu_2 * delta_m * I_i2
    dR2   <- ( gamma_R * I_l2 ) + ( (1 - pi_2) * gamma_h * I_h2 ) + ( (1 - mu_2) * gamma_i * I_i2 )
    
    list(c(dS2, dE2, dI2, dI_l2, dI_h2, dI_i2, dM2, dR2))
    
  })
}

## Tiempo ====

t <- seq (0, 100, by = 0.01)

## Parametros ====

parameters <- c(
  
  beta_2  <- 0.4473864     ,
  
  alpha   <- 1/5.6         ,
  
  pl_2    <- 0.9610823     ,
  
  ph_2    <- 0.03891768    ,
  
  delta_l <- 1/5.5         ,
  
  delta_h <- 1/4           ,
  
  gamma_R <- 1/14          ,
  
  pi_2    <- 0.11152620    ,
  
  delta_i <- 1/1           ,
  
  gamma_h <- 1/12          ,
  
  mu_2    <- 0.6896226     ,
  
  delta_m <- 1/8           ,
  
  gamma_i <- 1/7           ,
  
  N       <- 801000
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
  S2    = N - 1,
  E2    = 0,
  I2    = 1,
  I_l2  = 0,
  I_h2  = 0,
  I_i2  = 0,
  M2    = 0,
  R2    = 0
)

## Out ====
out <- as.data.frame(ode(y     = state, 
                         times = t, 
                         func   = modelo_covid_grupo_2,
                         parms = parameters))

## Grafica ====
pdf("03_Out/Plots/Modelo COVID del Grupo 2 para el Estado de Queretaro.pdf",
    paper = "a4r", width = 12, height = 9)
matplot(out[,1], out[,2:9], type ="l", xlab = "tiempo", ylab = "Población", 
        main = "Modelo COVID del Grupo 2 para el estado de Queretaro", 
        lwd = 2, lty = 1, col = viridis(8)) 

legend("right", c("Suceptibles"                             ,
                  "Expuestos"                               ,
                  "Infectados"                              ,
                  "Contagiados sintomáticos leves"          ,
                  "Contagiados sintomáticos Hospitalizados" ,
                  "Unidad de Terapia Intensiva"             ,
                  "Muertos"                                 ,
                  "Recuperados"                             )
       , col = viridis(8), fill = viridis(8), cex = 1)
dev.off()