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

        
    })
}