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



# Definicion de parametros ====
## Parametros obtenidos por estructura de edad ====

# ---------------- Grupo 1: Personas menores de 18 años
# ---------------- Grupo 2: Personas de 18 - 29 & 30 - 39
# ---------------- Grupo 3: Personas de 40 - 49 & 50 - 59
# ---------------- Grupo 4: Personas de 60 - 69 & Personas mayores de 70 años

### Suceptible a Infectado ( S -> I )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
SI_18_minor     <- probabilidades_de_transicion [ 1, 1 ]
SI_18_30        <- mean (probabilidades_de_transicion [ 2:3, 1])
SI_40_59        <- mean (probabilidades_de_transicion [ 4:5, 1])
SI_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 1])


### Infectado a Infectado Leve ( I -> L )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
IL_18_minor     <- probabilidades_de_transicion [ 1, 2 ]
IL_18_30        <- mean (probabilidades_de_transicion [ 2:3, 2])
IL_40_59        <- mean (probabilidades_de_transicion [ 4:5, 2])
IL_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 2])


### Infectado a Infectado Grave/Hospitalizado ( I -> G )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
IG_18_minor     <- probabilidades_de_transicion [ 1, 3 ]
IG_18_30        <- mean (probabilidades_de_transicion [ 2:3, 3])
IG_40_59        <- mean (probabilidades_de_transicion [ 4:5, 3])
IG_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 3])


### Infectado Grave/Hospitalizado a Intubado/ICU ( G -> ICU )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
GICU_18_minor     <- probabilidades_de_transicion [ 1, 4 ]
GICU_18_30        <- mean (probabilidades_de_transicion [ 2:3, 4])
GICU_40_59        <- mean (probabilidades_de_transicion [ 4:5, 4])
GICU_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 4])


### Intubado/ICU a Muerte ( ICU -> M )
# Para poder determinar el parametro solamente para los cuatro grupos etarios
# de los siete rangos de edades, se determinan los promedios de acuerdo 
# al parametro que se esta obteniendo
ICUM_18_minor     <- probabilidades_de_transicion [ 1, 5 ]
ICUM_18_30        <- mean (probabilidades_de_transicion [ 2:3, 5])
ICUM_40_59        <- mean (probabilidades_de_transicion [ 4:5, 5])
ICUM_60_70_higher <- mean (probabilidades_de_transicion [ 6:7, 5])

# Para poder visualizar estos resultados, se elabora una tabla
SI <- c(SI_18_minor, SI_18_30, SI_40_59, SI_60_70_higher)
IL <- c(IL_18_minor, IL_18_30, IL_40_59, IL_60_70_higher)
IG <- c(IG_18_minor, IG_18_30, IG_40_59, IG_60_70_higher)
GICU <- c(GICU_18_minor, GICU_18_30, GICU_40_59, GICU_60_70_higher)
ICUM <- c(ICUM_18_minor, ICUM_18_30, ICUM_40_59, ICUM_60_70_higher)

parms_estructura_edad <- cbind(SI, IL, IG, GICU, ICUM)
colnames(parms_estructura_edad) <- c("S -> I", "I -> L", "I -> H", "H -> ICU",
                                     "ICU -> M")
rownames(parms_estructura_edad) <- c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4")
# Se guarda el objeto como un .RData
# save(parms_estructura_edad, file = "03_Out/OutData/Tabla de parametros obtendos por estructura de edad.RData")



## Parametros del modelo ====

### Parámetros definidos del modelo de la CDMX. Ajustados a datos actuales.
Dincub  <- 5.6  # Tiempo de incubación (dias) (Quesada et al., 2021 DOI: 10.1016/j.rceng.2020.08.002) === ✓ 
Dinfect <- 5.5  # Tiempo en el que es infeccioso el paciente (Xin et al., 2021 https://doi.org/10.1093/cid/ciab746) === ✓ 
DRL     <- 14   # Tiempo de recuperacion de casos leves (dias) #Se mantiene de diversas fuentes === ✓ 
Dhosp   <- 4    # Tiempo entre presencia de sintomas y hospitalización en casos graves (dias) === ✓ 
DRH     <- 12   # Tiempo entre hospitalizacion de casos gravez no UCI y recuperacion (dias) === ✓ 
DM      <- 8    # Tiempo entre ingreso a UCI y deceso === ✓ 
DRICU   <- 7    # Tiempo entre ingreso a UCI y recuperación (dias) === ✓ 
DICU    <- 1    # Tiempo entre ingreso a hospitalización e ingreso a UCI === ✓ 
TIME <- 3       # Numero de años a simular
N <- 2368467    # Poblacion del estado de Queretaro https://cuentame.inegi.org.mx/monografias/informacion/queret/poblacion/default.aspx?tema 

### Asignacion de parametros

# beta. Estructura de Edad (EE)
beta_1 <- SI_18_minor
beta_2 <- parms_estructura_edad[2,1]
beta_3 <- parms_estructura_edad[3,1]
beta_4 <- parms_estructura_edad[4,1]


# alpha. Modelo CDMX. Tiempo en que individuos expuestos se vuelven infectantes.  
alpha <- 1/Dincub

# pl. Probabilidad de ser infectado leve/ambulatorio. EE.
pl_1 <- parms_estructura_edad[1,2]
pl_2 <- parms_estructura_edad[2,2]
pl_3 <- parms_estructura_edad[3,2]
pl_4 <- parms_estructura_edad[4,2]

# ph. Probabilidad de ser infectado grave/hospitalizado. EE.
ph_1 <- parms_estructura_edad[1,3]
ph_2 <- parms_estructura_edad[2,3]
ph_3 <- parms_estructura_edad[3,3]
ph_4 <- parms_estructura_edad[4,3]

# delta_l. Modelo CDMX. Tiempo en que los individuos Infectados se vuelven Infectados Leves
delta_l <- 1/Dinfect

# delta_h. Modelo CDMX. Tiempo en que los individuos Infectados se vuelven Infectados Graves/Hospitalizados
delta_h <- 1/Dhosp

# gamma_R. Modelo CDMX. Tiempo en que los individuos Infectados Leves se Recuperan
gamma_R <- 1/DRL

# pi. Probabilidad de que los individuos Hospitalizados sean Intubados (ICU). EE.
pi_1 <- parms_estructura_edad[1,4]
pi_2 <- parms_estructura_edad[2,4]
pi_3 <- parms_estructura_edad[3,4]
pi_4 <- parms_estructura_edad[4,4]

# delta_i. Modelo CDMX. Tiempo en que los individuos Hospitalizados son Intubados (ICU).
delta_i <- 1/DICU

# gamma_h. Modelo CDMX. Tiempo en que los individuos Hospitalizados se Recuperan.
gamma_h <- 1/DRH

# mu. Probabilidad de que los individuos Intubados (ICU) mueran. EE.
mu_1 <- parms_estructura_edad[1,5]
mu_2 <- parms_estructura_edad[2,5]
mu_3 <- parms_estructura_edad[3,5]
mu_4 <- parms_estructura_edad[4,5]

# delta_m. Modelo CDMX. Tiempo en que los individuos Intubados (ICU) mueren.
delta_m <- 1/DM

# gamma_i. Modelo CDMX. Tiempo en que los individuos Intubados (ICU) se Recuperan.
gamma_i <- 1/DRICU




# Resolucion ====
## Funcion del modelo ====
modelo_covid <- function(t, x, params) {
    
    S1   <- x[1] # Suceptibles                grupo 1
    E1   <- x[2] # Expuestos                  grupo 1
    I1   <- x[3] # Infectados                 grupo 1
    I_l1 <- x[4] # Infectados leves           grupo 1
    I_h1 <- x[5] # Infectados hospitalizados  grupo 1
    I_i1 <- x[6] # Infectados intubados       grupo 1
    R1   <- x[7] # Recuperados                grupo 1
    M1   <- x[8] # Fallecimientos             grupo 1
    
    
    S2   <- x[9]  # Suceptibles                grupo 2
    E2   <- x[10] # Expuestos                  grupo 2
    I2   <- x[11] # Infectados                 grupo 2
    I_l2 <- x[12] # Infectados leves           grupo 2
    I_h2 <- x[13] # Infectados hospitalizados  grupo 2
    I_i2 <- x[14] # Infectados intubados       grupo 2
    R2   <- x[15] # Recuperados                grupo 2
    M2   <- x[16] # Fallecimientos             grupo 2
    
    
    S3   <- x[17] # Suceptibles                grupo 3
    E3   <- x[18] # Expuestos                  grupo 3
    I3   <- x[19] # Infectados                 grupo 3
    I_l3 <- x[20] # Infectados leves           grupo 3
    I_h3 <- x[21] # Infectados hospitalizados  grupo 3
    I_i3 <- x[22] # Infectados intubados       grupo 3
    R3   <- x[23] # Recuperados                grupo 3
    M3   <- x[24] # Fallecimientos             grupo 3
    
    
    S4   <- x[25] # Suceptibles                grupo 4
    E4   <- x[26] # Expuestos                  grupo 4
    I4   <- x[27] # Infectados                 grupo 4
    I_l4 <- x[28] # Infectados leves           grupo 4
    I_h4 <- x[29] # Infectados hospitalizados  grupo 4
    I_i4 <- x[30] # Infectados intubados       grupo 4
    R4   <- x[31] # Recuperados                grupo 4
    M4   <- x[32] # Fallecimientos             grupo 4
    
    with(as.list(params), 
         
         {
             
             ## GRUPO 1
             S1   <- - beta_1 * ( S1 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N )
             E1   <- beta_1 * ( S1 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N ) - ( alpha * E1 )
             I1   <- ( alpha * E1 ) - ( ph_1 * delta_h * I1) - ( pl_1 * delta_l * I1)
             I_l1 <- ( pl_1 * delta_l * I1) - ( gamma_R * I_l1 )
             I_h1 <- ( ph_1 * delta_h * I1) - ( pi_1 * delta_i * I_h1 ) - ( (1 - pi_1) * gamma_h * I_h1 )
             I_i1 <- ( pi_1 * delta_i * I_h1 ) - ( mu_1 * delta_m * I_i1 ) - ( (1 - mu_1) * gamma_i * I_i1 )
             M1   <-  mu_1 * delta_m * I_i1
             R1   <- ( gamma_R * I_l1 ) + ( (1 - pi_1) * gamma_h * I_h1 ) + ( (1 - mu_1) * gamma_i * I_i1 )
             
             
             
             ## GRUPO 2
             S2   <- - beta_2 * ( S2 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N )
             E2   <- beta_2 * ( S2 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N ) - ( alpha * E2 )
             I2   <- ( alpha * E2 ) - ( ph_2 * delta_h * I2) - ( pl_2 * delta_l * I2)
             I_l2 <- ( pl_2 * delta_l * I2) - ( gamma_R * I_l2 )
             I_h2 <- ( ph_2 * delta_h * I2) - ( pi_2 * delta_i * I_h2 ) - ( (1 - pi_2) * gamma_h * I_h2 )
             I_i2 <- ( pi_2 * delta_i * I_h2 ) - ( mu_2 * delta_m * I_i2 ) - ( (1 - mu_2) * gamma_i * I_i2 )
             M2   <-  mu_2 * delta_m * I_i2
             R2   <- ( gamma_R * I_l2 ) + ( (1 - pi_2) * gamma_h * I_h2 ) + ( (1 - mu_2) * gamma_i * I_i2 )
             
             
             
             ## GRUPO 3
             S3   <- - beta_3 * ( S3 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N )
             E3   <- beta_3 * ( S3 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N ) - ( alpha * E3 )
             I3   <- ( alpha * E3 ) - ( ph_3 * delta_h * I3) - ( pl_3 * delta_l * I3)
             I_l3 <- ( pl_3 * delta_l * I3) - ( gamma_R * I_l3 )
             I_h3 <- ( ph_3 * delta_h * I3) - ( pi_3 * delta_i * I_h3 ) - ( (1 - pi_3) * gamma_h * I_h3 )
             I_i3 <- ( pi_3 * delta_i * I_h3 ) - ( mu_3 * delta_m * I_i3 ) - ( (1 - mu_3) * gamma_i * I_i3 )
             M3   <-  mu_3 * delta_m * I_i3
             R3   <- ( gamma_R * I_l3 ) + ( (1 - pi_3) * gamma_h * I_h3 ) + ( (1 - mu_3) * gamma_i * I_i3 )
             
             
             
             ## GRUPO 4
             S4   <- - beta_4 * ( S4 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N )
             E4   <- beta_4 * ( S4 * (I1 + I2 + I3 + I4 + I_l1 + I_l2 + I_l3 + I_l4 ) / N ) - ( alpha * E4 )
             I4   <- ( alpha * E4 ) - ( ph_4 * delta_h * I4) - ( pl_4 * delta_l * I4)
             I_l4 <- ( pl_4 * delta_l * I4) - ( gamma_R * I_l4 )
             I_h4 <- ( ph_4 * delta_h * I4) - ( pi_4 * delta_i * I_h4 ) - ( (1 - pi_4) * gamma_h * I_h4 )
             I_i4 <- ( pi_4 * delta_i * I_h4 ) - ( mu_4 * delta_m * I_i4 ) - ( (1 - mu_4) * gamma_i * I_i4 )
             M4   <-  mu_4 * delta_m * I_i4
             R4   <- ( gamma_R * I_l4 ) + ( (1 - pi_4) * gamma_h * I_h4 ) + ( (1 - mu_4) * gamma_i * I_i4 ) 
             
             
             
             dx <- c(S1, E1, I1, I_l1, I_h1, I_i1, R1, M1,
                     S2, E2, I2, I_l2, I_h2, I_i2, R2, M2,
                     S3, E3, I3, I_l3, I_h3, I_i3, R3, M3,
                     S4, E4, I4, I_l4, I_h4, I_i4, R4, M4)
             list(dx)
             
         }
    )
}
    







## Times, Parametros, Variables ====
# Tiempo
times <- seq (0, 1000, by = 0.01)

# Parámetros
params <- c(
    
    beta_1  = beta_1  ,
    beta_2  = beta_2  ,
    beta_3  = beta_3  ,
    beta_4  = beta_4  ,
    
    alpha   = alpha   , 
    
    pl_1    = pl_1    ,
    pl_2    = pl_2    ,
    pl_3    = pl_3    ,
    pl_4    = pl_4    ,
    
    ph_1    = ph_1    ,
    ph_2    = ph_2    ,
    ph_3    = ph_3    ,
    ph_4    = ph_4    ,
    
    delta_l = delta_l , 
    
    delta_h = delta_h ,
    
    gamma_R = gamma_R ,
    
    pi_1    = pi_1    ,
    pi_2    = pi_2    ,
    pi_3    = pi_3    ,
    pi_4    = pi_4    ,
    
    delta_i = delta_i ,
    
    gamma_h = gamma_h ,
    
    mu_1    = mu_1    ,
    mu_2    = mu_2    ,
    mu_3    = mu_3    ,
    mu_4    = mu_4    ,
    
    delta_m = delta_m ,
    
    gamma_i = gamma_i
    
)

#Condiciones iniciales del sistema
# De acuerdo a la información obtenida del INEGI, la población del estado de 
# Querétaro es de 2,368,467 habitantes, al senso realizado en 2020. 
# Este valor de la poblacion se toma como el valor total de la población.
# Para el caso de cada estructura de edad definia, la población de individuos 
# suceptibles quedaria como:
#        Grupo 1: Menores de 18 años = 782000
#        Grupo 2: 18 - 39 años = 801000
#        Grupo 3: 40 - 59 años = 539000
#        Grupo 4: 60 - >70 años = 242000

xstart <- c( 
    
    S1    = 782000 - 2,
    E1    = 0,
    I1    = 1,
    I_l1  = 1,
    I_h1  = 0,
    I_i1  = 0,
    R1    = 0,
    M1    = 0,
    
    S2    = 801000 - 2,
    E2    = 0,
    I2    = 1,
    I_l2  = 1,
    I_h2  = 0,
    I_i2  = 0,
    R2    = 0,
    M2    = 0,
    
    S3    = 539000 - 2,
    E3    = 0,
    I3    = 1,
    I_l3  = 1,
    I_h3  = 0,
    I_i3  = 0,
    R3    = 0,
    M3    = 0,
    
    S4    = 242000 - 2,
    E4    = 0,
    I4    = 1,
    I_l4  = 1,
    I_h4  = 0,
    I_i4  = 0,
    R4    = 0,
    M4    = 0
    
)

## Out ====
out <- as.data.frame(ode(y     = xstart, 
                         times = times, 
                         func   = modelo_covid,
                         parms = params,
                         method = "euler"))
out





## Grafica ====
pdf("03_Out/Plots/Modelo COVID por Estructura Etaria para el Estado de Queretaro.pdf")
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
       , col = 1:33, lty = 1, cex = 0.5)
dev.off()