# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(lubridate)
library(ggmatplot)
library(simecol)
library(plotly)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")


# Se carga la base de datos
load("03_Out/OutData/probabilidades_de_transicion.RData")
load("03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")
contact_rate_df <- read.csv("01_RawData/contact_rate_qro.csv")

# Resolucion =
## Funcion del modelo ----------------------------------------------------------
beta_t_modelo_covid <- function(time, state, parameters){
  
  with(as.list(c(state, parameters)), {
    ## GRUPO 1
    # Total population
    total_pop <- S1 + E1 + I1 + I_l1 + I_h1 + I_i1 + R1 + S2 + E2 + I2 +  I_l2 + I_h2 + I_i2 + R2 + S3 + E3 + I3 + I_l3 + I_h3 + I_i3 + R3 + S4 + E4 + I4 + I_l4 + I_h4 + I_i4 + R4  
    
    dS1   <- - S1 *(I1 + I2 + I3 + I4)/(total_pop)*contact_rate(time)+ immun_lost*R1
    dE1   <-  S1 * (I1 + I2 + I3 + I4)/(total_pop) *contact_rate(time) - ( alpha * E1 )
    dI1   <- ( alpha * E1 ) - ( ph_1 * delta_h * I1 ) - ( pl_1 * delta_l * I1 )
    dI_l1 <- ( pl_1 * delta_l * I1 ) - ( gamma_R * I_l1 )
    dI_h1 <- ( ph_1 * delta_h * I1 ) - ( pi_1 * delta_i * I_h1 ) - ( (1 - pi_1) * gamma_h * I_h1 )
    dI_i1 <- ( pi_1 * delta_i * I_h1 ) - ( mu_1 * delta_m * I_i1 ) - ( (1 - mu_1) * gamma_i * I_i1 )
    dM1   <-  mu_1 * delta_m * I_i1
    dR1   <- ( gamma_R * I_l1 ) + ( (1 - pi_1) * gamma_h * I_h1 ) + ( (1 - mu_1) * gamma_i * I_i1 ) - immun_lost*R1
    
    
    
    ## GRUPO 2
    dS2   <- - S2 *contact_rate(time) * (I1 + I2+ I3 + I4)/(total_pop)+ immun_lost*R2
    dE2   <-   contact_rate(time) * S2 * (I1 + I2 + I3 + I4)/(total_pop)  - ( alpha * E2 )
    dI2   <- ( alpha * E2 ) - ( ph_2 * delta_h * I2 ) - ( pl_2 * delta_l * I2 )
    dI_l2 <- ( pl_2 * delta_l * I2 ) - ( gamma_R * I_l2 )
    dI_h2 <- ( ph_2 * delta_h * I2 ) - ( pi_2 * delta_i * I_h2 ) - ( (1 - pi_2) * gamma_h * I_h2 )
    dI_i2 <- ( pi_2 * delta_i * I_h2 ) - ( mu_2 * delta_m * I_i2 ) - ( (1 - mu_2) * gamma_i * I_i2 )
    dM2   <-  mu_2 * delta_m * I_i2
    dR2   <- ( gamma_R * I_l2 ) + ( (1 - pi_2) * gamma_h * I_h2 ) + ( (1 - mu_2) * gamma_i * I_i2 )-immun_lost*R2
    
    
    
    ## GRUPO 3
    dS3   <- - S3*contact_rate(time) *(I1 + I2 + I3 + I4)/(total_pop)+ immun_lost*R3
    dE3   <-   contact_rate(time) * S3 * (I1 + I2 + I3 + I4)/(total_pop) - ( alpha * E3 )
    dI3   <- ( alpha * E3 ) - ( ph_3 * delta_h * I3 ) - ( pl_3 * delta_l * I3 )
    dI_l3 <- ( pl_3 * delta_l * I3 ) - ( gamma_R * I_l3 )
    dI_h3 <- ( ph_3 * delta_h * I3 ) - ( pi_3 * delta_i * I_h3 ) - ( (1 - pi_3) * gamma_h * I_h3 )
    dI_i3 <- ( pi_3 * delta_i * I_h3 ) - ( mu_3 * delta_m * I_i3 ) - ( (1 - mu_3) * gamma_i * I_i3 )
    dM3   <-  mu_3 * delta_m * I_i3
    dR3   <- ( gamma_R * I_l3 ) + ( (1 - pi_3) * gamma_h * I_h3 ) + ( (1 - mu_3) * gamma_i * I_i3 )-immun_lost*R3
    
    
    
    ## GRUPO 4
    dS4   <- - S4  * (I1 + I2 + I3 + I4)/(total_pop)* contact_rate(time) + immun_lost*R4
    dE4   <-   contact_rate(time)* S4 * (I1 + I2 + I3 + I4)/(total_pop)  - ( alpha * E4 )
    dI4   <- ( alpha * E4 ) - ( ph_4 * delta_h * I4 ) - ( pl_4 * delta_l * I4 )
    dI_l4 <- ( pl_4 * delta_l * I4 ) - ( gamma_R * I_l4 )
    dI_h4 <- ( ph_4 * delta_h * I4 ) - ( pi_4 * delta_i * I_h4 ) - ( (1 - pi_4) * gamma_h * I_h4 )
    dI_i4 <- ( pi_4 * delta_i * I_h4 ) - ( mu_4 * delta_m * I_i4 ) - ( (1 - mu_4) * gamma_i * I_i4 )
    dM4   <-  mu_4 * delta_m * I_i4
    dR4   <- ( gamma_R * I_l4 ) + ( (1 - pi_4) * gamma_h * I_h4 ) + ( (1 - mu_4) * gamma_i * I_i4 )- immun_lost*R4
    
    
    
    list(c(dS1, dE1, dI1, dI_l1, dI_h1, dI_i1, dM1, dR1,
           dS2, dE2, dI2, dI_l2, dI_h2, dI_i2, dM2, dR2,
           dS3, dE3, dI3, dI_l3, dI_h3, dI_i3, dM3, dR3,
           dS4, dE4, dI4, dI_l4, dI_h4, dI_i4, dM4, dR4))
    
  })
}

##funciones beta_t -------------------------------------------------------------
contact_rate<-function(time){
  return(contact_rate_df[time,3])
}

## Tiempo ----------------------------------------------------------------------

time <- seq(1,398,by=1)

## Parametros ------------------------------------------------------------------
parameters <- c(
  
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
  
  immun_lost<- 1/180       ,
  
  N1       <- 782000       ,
  N2       <- 801000       ,
  N3       <- 539000       ,
  N4       <- 242000       
  
)

## Condiciones iniciales del sistema -------------------------------------------
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
  
  ## GRUPO 1
  S1    = N1 - 1,
  E1    = 0,
  I1    = 1,
  I_l1  = 0,
  I_h1  = 0,
  I_i1  = 0,
  M1    = 0,
  R1    = 0,
  
  
  
  ## GRUPO 2
  S2    = N2 - 1,
  E2    = 0,
  I2    = 1,
  I_l2  = 0,
  I_h2  = 0,
  I_i2  = 0,
  M2    = 0,
  R2    = 0,
  
  ## GRUPO 3
  S3    = N3 - 1,
  E3    = 0,
  I3    = 1,
  I_l3  = 0,
  I_h3  = 0,
  I_i3  = 0,
  M3    = 0,
  R3    = 0,
  
  
  
  ## GRUPO 4
  S4    = N4 - 1,
  E4    = 0,
  I4    = 1,
  I_l4  = 0,
  I_h4  = 0,
  I_i4  = 0,
  M4    = 0,
  R4    = 0
)

## Out -------------------------------------------------------------------------
beta_t_out <- as.data.frame(ode(y     = state,
                                times = time,
                                func  = beta_t_modelo_covid,
                                parms = parameters))

## Grafica ---------------------------------------------------------------------
# beta_t_grafica_modelo <- ggmatplot(x = beta_t_out[,1],
#                             y = beta_t_out[,2:33],
#                             plot_type = "line",
#                             fill =viridis(32),
#                             linetype = 1, xlab = "Tiempo", ylab = "Población",
#                             main = "Modelo COVID beta_t",
#                             legend_title = "Variables", lwd = 1) +
#   theme(panel.background = element_rect(),
#         plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
#         axis.line = element_line(colour = "black", size = 0.65),
#         axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
#         axis.text.y = element_text(size = 9, face = "bold"),
#         axis.title.x = element_text(size = 12, face = "bold"),
#         axis.title.y = element_text(size = 12, face = "bold"),
#         legend.position = "right",  # Posición de la leyenda
#         legend.title = element_text(size = 10, face = "bold"),  # Título de la leyenda
#         legend.text = element_text(size = 10),  # Texto de la leyenda
#         legend.spacing = unit(0.5, "cm"))
beta_t_grafica_modelo

# jpeg("03_Out/Plots/Modelo COVID con beta t.jpeg",
#     width = 5733, height = 4300, res = 500, units = "px")
# beta_t_grafica_modelo
# dev.off()