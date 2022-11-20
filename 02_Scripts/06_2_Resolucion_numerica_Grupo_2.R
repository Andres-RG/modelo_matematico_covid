# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)



covid <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {

    dS   <- - beta * S * I
    dE   <- (beta * S * I) - ( alpha * E )
    dI   <- ( alpha * E ) - ( ph * delta_h * I) - ( pl * delta_l * I)
    dI_l <- ( pl * delta_l * I) - ( gamma_R * I_l )
    dI_h <- ( ph * delta_h * I) - ( pi * delta_i * I_h ) - ( (1 - pi) * gamma_h * I_h )
    dI_i <- ( pi * delta_i * I_h ) - ( mu * delta_m * I_i ) - ( (1 - mu) * gamma_i * I_i )
    dM   <-  mu * delta_m * I_i
    dR   <- ( gamma_R * I_l ) + ( (1 - pi) * gamma_h * I_h ) + ( (1 - mu) * gamma_i * I_i )
    
    
    list(c(dS, dE, dI, dI_l, dI_h, dI_i, dR, dM))
  })
}

parameters <- c(
  
  beta  <- 0.3771645     ,
  
  alpha   <- 1/5.6         ,
  
  pl    <- 0.9746533     ,
  
  ph    <- 0.02534672    ,
  
  delta_l <- 1/5.5         ,
  
  delta_h <- 1/4           ,
  
  gamma_R <- 1/14          ,
  
  pi    <- 0.09433962    ,
  
  delta_i <- 1/1           ,
  
  gamma_h <- 1/12          ,
  
  mu    <- 0.5000000     ,
  
  delta_m <- 1/8           ,
  
  gamma_i <- 1/7           ,
  
  N       <- 782000
)

state <- c(
  S    = 782000 - 1,
  E    = 0,
  I    = 1,
  I_l  = 0,
  I_h  = 0,
  I_i  = 0,
  R    = 0,
  M    = 0
)

t <- seq (0, 100, by = 0.01)

out <- ode(state, t, covid, parameters)

matplot(out[,1], out[,2:9], type ="l", xlab = "tiempo", ylab = "Población", main = "Modelo COVID por estructura etaria para el estado de Queretaro", lwd = 2, lty = 1)
legend("topright", c("Suceptibles"                             ,
                     "Expuestos"                               ,
                     "Infectados"                              ,
                     "Contagiados sintomáticos leves"          ,
                     "Contagiados sintomáticos Hospitalizados" ,
                     "Unidad de Terapia Intensiva"             ,
                     "Recuperados"                            ,
                     "Muertos"                                 )
       , col =1:9,lty = 1, cex = 0.5)