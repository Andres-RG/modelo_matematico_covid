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
modelo_covid_grupo_1 <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    
    ## GRUPO 1
    dS1   <- - beta_1 * S1 * I1
    dE1   <- (beta_1 * S1 * I1) - ( alpha * E1 )
    dI1   <- ( alpha * E1 ) - ( ph_1 * delta_h * I1 ) - ( pl_1 * delta_l * I1 )
    dI_l1 <- ( pl_1 * delta_l * I1 ) - ( gamma_R * I_l1 )
    dI_h1 <- ( ph_1 * delta_h * I1 ) - ( pi_1 * delta_i * I_h1 ) - ( (1 - pi_1) * gamma_h * I_h1 )
    dI_i1 <- ( pi_1 * delta_i * I_h1 ) - ( mu_1 * delta_m * I_i1 ) - ( (1 - mu_1) * gamma_i * I_i1 )
    dM1   <-  mu_1 * delta_m * I_i1
    dR1   <- ( gamma_R * I_l1 ) + ( (1 - pi_1) * gamma_h * I_h1 ) + ( (1 - mu_1) * gamma_i * I_i1 )
    
    list(c(dS1, dE1, dI1, dI_l1, dI_h1, dI_i1, dM1, dR1))
    
  })
}

## Tiempo ====

t <- seq (0, 100, by = 0.01)

## Parametros ====

parameters <- c(
  
  beta_1  <- 0.3771645     ,
  
  alpha   <- 1/5.6         ,
  
  pl_1    <- 0.9746533     ,
  
  ph_1    <- 0.02534672    ,
  
  delta_l <- 1/5.5         ,
  
  delta_h <- 1/4           ,
  
  gamma_R <- 1/14          ,
  
  pi_1    <- 0.09433962    ,
  
  delta_i <- 1/1           ,
  
  gamma_h <- 1/12          ,
  
  mu_1    <- 0.5000000     ,
  
  delta_m <- 1/8           ,
  
  gamma_i <- 1/7           ,
  
  N       <- 782000
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
  S1    = N - 1,
  E1    = 0,
  I1    = 1,
  I_l1  = 0,
  I_h1  = 0,
  I_i1  = 0,
  M1    = 0,
  R1    = 0
)

## Out ====
out <- as.data.frame(ode(y     = state, 
                         times = t, 
                         func   = modelo_covid_grupo_1,
                         parms = parameters))
out

## Grafica ====
pdf("03_Out/Plots/Modelo COVID del Grupo 1 para el Estado de Queretaro.pdf")
matplot(out[,1], out[,2:9], type ="l", xlab = "tiempo", ylab = "Población", 
        main = "Modelo COVID del Grupo 1 para el estado de Queretaro", 
        lwd = 2, lty = 1, col = 1:8) 
  
legend("right", c("Suceptibles"                             ,
                  "Expuestos"                               ,
                  "Infectados"                              ,
                  "Contagiados sintomáticos leves"          ,
                  "Contagiados sintomáticos Hospitalizados" ,
                  "Unidad de Terapia Intensiva"             ,
                  "Muertos"                                 ,
                  "Recuperados"                             )
       , col = 1:8,lty = 1, cex = 0.7)
dev.off()

