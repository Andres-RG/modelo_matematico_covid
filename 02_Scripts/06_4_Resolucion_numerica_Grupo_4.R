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


# Se carga la base de datos
load("03_Out/OutData/probabilidades_de_transicion.RData")
load("03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")


# Resolucion ====
## Funcion del modelo ====
modelo_covid_grupo_4 <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
        
        ## GRUPO 4
        dS4   <- - beta_4 * S4 * I4
        dE4   <- (beta_4 * S4 * I4) - ( alpha * E4 )
        dI4   <- ( alpha * E4 ) - ( ph_4 * delta_h * I4 ) - ( pl_4 * delta_l * I4 )
        dI_l4 <- ( pl_4 * delta_l * I4 ) - ( gamma_R * I_l4 )
        dI_h4 <- ( ph_4 * delta_h * I4 ) - ( pi_4 * delta_i * I_h4 ) - ( (1 - pi_4) * gamma_h * I_h4 )
        dI_i4 <- ( pi_4 * delta_i * I_h4 ) - ( mu_4 * delta_m * I_i4 ) - ( (1 - mu_4) * gamma_i * I_i4 )
        dM4   <-  mu_4 * delta_m * I_i4
        dR4   <- ( gamma_R * I_l4 ) + ( (1 - pi_4) * gamma_h * I_h4 ) + ( (1 - mu_4) * gamma_i * I_i4 )
        
        list(c(dS4, dE4, dI4, dI_l4, dI_h4, dI_i4, dM4, dR4))
        
    })
}

## Tiempo ====

t <- seq (0, 100, by = 0.01)

## Parametros ====

parameters <- c(
    
    beta_4  <- 0.5638485     ,
    
    alpha   <- 1/5.6         ,
    
    pl_4    <- 0.5567019     ,
    
    ph_4    <- 0.44329812    ,
    
    delta_l <- 1/5.5         ,
    
    delta_h <- 1/4           ,
    
    gamma_R <- 1/14          ,
    
    pi_4    <- 0.21197680    ,
    
    delta_i <- 1/1           ,
    
    gamma_h <- 1/12          ,
    
    mu_4    <- 0.8972645     ,
    
    delta_m <- 1/8           ,
    
    gamma_i <- 1/7           ,
    
    N       <- 242000
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
    S4    = N - 1,
    E4    = 0,
    I4    = 1,
    I_l4  = 0,
    I_h4  = 0,
    I_i4  = 0,
    M4    = 0,
    R4    = 0
)

## Out ====
out_grupo_4 <- as.data.frame(ode(y     = state, 
                         times = t, 
                         func   = modelo_covid_grupo_4,
                         parms = parameters))

## Grafica ====
#pdf("03_Out/Plots/Modelo COVID del Grupo 4 para el Estado de Queretaro.pdf",
#    paper = "a4r", width = 12, height = 9)
matplot(out_grupo_4[,1], out_grupo_4[,2:9], type ="l", xlab = "tiempo", ylab = "Población", 
        main = "Modelo COVID del Grupo 4 para el estado de Queretaro", 
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
#dev.off()



#png("03_Out/Plots/Modelo COVID del Grupo 4 para el Estado de Queretaro.png",
#    width = 265, height = 225, res = 300, units = "mm")
matplot(out_grupo_4[,1], out_grupo_4[,2:9], type ="l", xlab = "tiempo", ylab = "Población", 
        main = "Modelo COVID del Grupo 4 para el estado de Queretaro", 
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
#dev.off()
