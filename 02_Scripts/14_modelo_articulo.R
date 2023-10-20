# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(lubridate)
library(ggmatplot)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")


#----------
# Modelo.
# A methodology to generate epidemic scenarios for emerging
# infectious diseases based on the use of key calendar events.

m <- function(t, state, parameters){
    with(as.list(c(state, parameters)),{
        
        dS = (omega * R) + (fi * V) + (x * M) - (((beta_t) * (Y + q * I) * S)/(N)) - ((x + psi) * S)
        dV = (psi * S) - ((1 - sigma) * ((beta_t) * (Y + q * I) * V)/(N)) - ((fi + x) * V)
        dE = (((beta_t) * (Y + q * I) * S)/(N)) + ((1 - sigma) * ((beta_t) * (Y + q * I) * V)/(N)) - ((gamma + x) * E)
        dI = (rho * gamma * E) - ((delta + x) * I)
        dY = ((1 - rho) * gamma * E) - (((epsilon * eta) + (1 - epsilon) * ni + x) * Y )
        dT_rep = ((1 - epsilon) * ni * Y) - (((a(t) * k) + (1 - a(t))* mu + x )* T_rep )
        dR = (delta * I) + (epsilon * eta * Y) + (a(t) * k * T_rep) - ((omega + x) * R)
        dD = (1 - a(t)) * mu * T_rep
        
        list(c(dS, dV, dE, dI, dY, dT_rep, dR, dD))

        
    })
}

t <- seq(1,600, by = 1)

parameters <- c(
  
  psi <- -log(1 - 0.94)/371, #https://coronavirus.gob.mx/wp-content/uploads/2022/02/2022.02.07_CP_Salud_CTD_COVID-19.pdf
  ni <- ,
  fi <- 1/166,
  omega <- 0.006,
  gamma <- 0.196,
  delta <- 0.143,
  eta <- 0.071,
  k <- 0.1,
  x <- 0.00003629,
  rho <- 0.35,
  epsilon <- 0.95,
  q <- 0.45,
  
  
)