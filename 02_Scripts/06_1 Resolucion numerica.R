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
    ## GRUPO 1
    S1   <- - ( beta_1 / N ) * S1 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 )
    E1   <- ( ( beta_1 / N ) * S1 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) ) - ( alpha * E1 )
    I1   <- ( alpha * E1 ) - ( ph_1 * delta_h * I1) - ( pl_1 * delta_l * I1)
    I_l1 <- ( pl_1 * delta_l * I1) - ( gamma_R * I_l1 )
    I_h1 <- ( ph_1 * delta_h * I1) - ( pi_1 * delta_i * I_h1 ) - ( (1 - pi_1) * gamma_h * I_h1 )
    I_i1 <- ( pi_1 * delta_i * I_h1 ) - ( mu_1 * delta_m * I_i1 ) - ( (1 - mu_1) * gamma_i * I_i1 )
    M1   <-  mu_1 * delta_m * I_i1
    R1   <- ( gamma_R * I_l1 ) + ( (1 - pi_1) * gamma_h * I_h1 ) + ( (1 - mu_1) * gamma_i * I_i1 )
    
    
    
    ## GRUPO 2
    S2   <- - ( beta_2 / N ) * S2 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 )
    E2   <- ( ( beta_2 / N ) * S2 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) ) - ( alpha * E2 )
    I2   <- ( alpha * E2 ) - ( ph_2 * delta_h * I2) - ( pl_2 * delta_l * I2)
    I_l2 <- ( pl_2 * delta_l * I2) - ( gamma_R * I_l2 )
    I_h2 <- ( ph_2 * delta_h * I2) - ( pi_2 * delta_i * I_h2 ) - ( (1 - pi_2) * gamma_h * I_h2 )
    I_i2 <- ( pi_2 * delta_i * I_h2 ) - ( mu_2 * delta_m * I_i2 ) - ( (1 - mu_2) * gamma_i * I_i2 )
    M2   <-  mu_2 * delta_m * I_i2
    R2   <- ( gamma_R * I_l2 ) + ( (1 - pi_2) * gamma_h * I_h2 ) + ( (1 - mu_2) * gamma_i * I_i2 )
    
    
    
    ## GRUPO 3
    S3   <- - ( beta_3 / N ) * S3 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 )
    E3   <- ( ( beta_3 / N ) * S3 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) ) - ( alpha * E3 )
    I3   <- ( alpha * E3 ) - ( ph_3 * delta_h * I3) - ( pl_3 * delta_l * I3)
    I_l3 <- ( pl_3 * delta_l * I3) - ( gamma_R * I_l3 )
    I_h3 <- ( ph_3 * delta_h * I3) - ( pi_3 * delta_i * I_h3 ) - ( (1 - pi_3) * gamma_h * I_h3 )
    I_i3 <- ( pi_3 * delta_i * I_h3 ) - ( mu_3 * delta_m * I_i3 ) - ( (1 - mu_3) * gamma_i * I_i3 )
    M3   <-  mu_3 * delta_m * I_i3
    R3   <- ( gamma_R * I_l3 ) + ( (1 - pi_3) * gamma_h * I_h3 ) + ( (1 - mu_3) * gamma_i * I_i3 )
    
    
    
    ## GRUPO 4
    S4   <- - ( beta_4 / N ) * S4 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 )
    E4   <- ( ( beta_4 / N ) * S4 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) ) - ( alpha * E4 )
    I4   <- ( alpha * E4 ) - ( ph_4 * delta_h * I4) - ( pl_4 * delta_l * I4)
    I_l4 <- ( pl_4 * delta_l * I4) - ( gamma_R * I_l4 )
    I_h4 <- ( ph_4 * delta_h * I4) - ( pi_4 * delta_i * I_h4 ) - ( (1 - pi_4) * gamma_h * I_h4 )
    I_i4 <- ( pi_4 * delta_i * I_h4 ) - ( mu_4 * delta_m * I_i4 ) - ( (1 - mu_4) * gamma_i * I_i4 )
    M4   <-  mu_4 * delta_m * I_i4
    R4   <- ( gamma_R * I_l4 ) + ( (1 - pi_4) * gamma_h * I_h4 ) + ( (1 - mu_4) * gamma_i * I_i4 )
    
    list(c(dS1, dE1, dI1, dI_l1, dI_h1, dI_i1, dR1, dM1,
           dS2, dE2, dI2, dI_l2, dI_h2, dI_i2, dR2, dM2,
           dS3, dE3, dI3, dI_l3, dI_h3, dI_i3, dR3, dM3,
           dS4, dE4, dI4, dI_l4, dI_h4, dI_i4, dR4, dM4 ))
  })
}


parameters <- c(
  beta_1  <- 0.3771645     ,
  beta_2  <- 0.4473864     ,
  beta_3  <- 0.5090940     ,
  beta_4  <- 0.5638485     ,
  
  alpha   <- 1/5.6         ,
  
  pl_1    <- 0.9746533     ,
  pl_2    <- 0.9610823     ,
  pl_3    <- 0.8420886     ,
  pl_4    <- 0.5567019     ,
  
  ph_1    <- 0.02534672    ,
  ph_2    <- 0.03891768    ,
  ph_3    <- 0.15791136    ,
  ph_4    <- 0.44329812    ,
  
  delta_l <- 1/5.5         ,
  
  delta_h <- 1/4           ,
  
  gamma_R <- 1/14          ,
  
  pi_1    <- 0.09433962    ,
  pi_2    <- 0.11152620    ,
  pi_3    <- 0.18827911    ,
  pi_4    <- 0.21197680    ,
  
  delta_i <- 1/1           ,
  
  gamma_h <- 1/12          ,
  
  mu_1    <- 0.5000000     ,
  mu_2    <- 0.6896226     ,
  mu_3    <- 0.8224699     ,
  mu_4    <- 0.8972645     ,
  
  delta_m <- 1/8           ,
  
  gamma_i <- 1/7           ,
  
  N       <- 2368467
)

state <- c(
  S1    = 782000 - 1,
  E1    = 0,
  I1    = 1,
  I_l1  = 0,
  I_h1  = 0,
  I_i1  = 0,
  R1    = 0,
  M1    = 0,
  
  S2    = 801000 - 1,
  E2    = 0,
  I2    = 1,
  I_l2  = 0,
  I_h2  = 0,
  I_i2  = 0,
  R2    = 0,
  M2    = 0,
  
  S3    = 539000 - 1,
  E3    = 0,
  I3    = 1,
  I_l3  = 0,
  I_h3  = 0,
  I_i3  = 0,
  R3    = 0,
  M3    = 0,
  
  S4    = 242000 - 1,
  E4    = 0,
  I4    = 1,
  I_l4  = 0,
  I_h4  = 0,
  I_i4  = 0,
  R4    = 0,
  M4    = 0
)

t <- seq (0, 100, by = 0.01)

out <- ode(state, t, covid, parameters)

matplot(out[,1], out[,2:33], type ="l", xlab = "tiempo", ylab = "Población", main = "Modelo COVID por estructura etaria para el estado de Queretaro", lwd = 2, lty = 1)
legend("topright", c("Suceptibles Grupo 1"                             ,
                     "Expuestos Grupo 1"                               ,
                     "Infectados Grupo 1"                              ,
                     "Contagiados sintomáticos leves Grupo 1"          ,
                     "Contagiados sintomáticos Hospitalizados Grupo 1" ,
                     "Unidad de Terapia Intensiva Grupo 1"             ,
                     "Recuperados  Grupo 1"                            ,
                     "Muertos Grupo 1"                                 ,
                     #
                     #
                     "Suceptibles Grupo 2"                             ,
                     "Expuestos Grupo 2"                               ,
                     "Infectados Grupo 2"                              ,
                     "Contagiados sintomáticos leves Grupo 2"          ,
                     "Contagiados sintomáticos Hospitalizados Grupo 2" ,
                     "Unidad de Terapia Intensiva Grupo 2"             ,
                     "Recuperados  Grupo 2"                            ,
                     "Muertos Grupo 2"                                 ,
                     #
                     #
                     "Suceptibles Grupo 3"                             ,
                     "Expuestos Grupo 3"                               ,
                     "Infectados Grupo 3"                              ,
                     "Contagiados sintomáticos leves Grupo 3"          ,
                     "Contagiados sintomáticos Hospitalizados Grupo 3" ,
                     "Unidad de Terapia Intensiva Grupo 3"             ,
                     "Recuperados  Grupo 3"                            ,
                     "Muertos Grupo 3"                                 ,
                     #
                     #
                     "Suceptibles Grupo 4"                             ,
                     "Expuestos Grupo 4"                               ,
                     "Infectados Grupo 4"                              ,
                     "Contagiados sintomáticos leves Grupo 4"          ,
                     "Contagiados sintomáticos Hospitalizados Grupo 4" ,
                     "Unidad de Terapia Intensiva Grupo 4"             ,
                     "Recuperados  Grupo 4"                            ,
                     "Muertos Grupo 4"                                 )
       , scale_fill_viridis(discrete = T), lty = 1, cex = 0.5)
