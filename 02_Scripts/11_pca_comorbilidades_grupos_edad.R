# Librerias necesarias 

library(ggplot2)
library(ggridges)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
library(ape)
library(lubridate)
library(ggmatplot)
library(devtools)
library(circlize)
library(plotly)
library(wesanderson)
library(dplyr)
library(ggfortify)
library(factoextra)
library(tidyr)
library(FactoMineR)
library(vcd)

 # Se carga la base de datos
load("03_Out/OutData/casos_positivos_rangos_edades.RData")

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# ANALISIS DE COMPONENTES PRINCIPALES
# 1. Se reduce la base de datos a solo las columnas necesarias
datos_varred <- select(casos_positivos_re, 
                       c("EDAD","DIABETES", "EPOC", "ASMA", "INMUSUPR",
                         "HIPERTENSION", "CARDIOVASCULAR", 
                         "OBESIDAD", "RENAL_CRONICA", 
                         "TABAQUISMO"))

# 2. Se añaden los rangos de edad pero solamente en terminos numericos
#    1 == Menores de 18 años
#    2 == 18 - 29 años
#    3 == 30 - 39 años
#    4 == 40 - 49 años
#    5 == 50 - 59 años
#    6 == 60 - 69 años
#    7 == Mayores de 70 años
renumeric <- rangos_edades_only_nums(datos_varred$EDAD)

# 2.1. Se añade la columna de rangos de edad a la base de datos
datos_varred_re <- mutate(datos_varred, RANGOS = renumeric)
# save(datos_varred, file = "03_Out/OutData/datos_positivos_reducidos.RData")
datos_varred <- datos_varred_re %>% select(c(-EDAD, -RANGOS))
datos_varred2 <- datos_varred[1:500,]

# 3. Estandarización de los datos
#    mean = 0 ; sd = 1
datos_estandarizados_varred <- scale(datos_varred)

# 3. PCA 
res.pca <- prcomp(datos_varred2, scale = TRUE)
summary(res.pca)

# 4. Plot
fviz_eig(res.pca) #scree plot

biplot(res.pca)

groups <- as.factor(datos_varred_re$RANGOS[1:500])
colores <- c("dodgerblue3", "springgreen3", "firebrick3", 
             "orange", "purple", "darkslategrey", "cyan")

p <- fviz_pca_ind(res.pca,
                  col.ind = groups, # color by groups
                  palette = colores,
                  addEllipses = TRUE, # Concentration ellipses
                  ellipse.type = "confidence",
                  legend.title = "Groups",
                  repel = TRUE
)
p


pca.plot <- autoplot(res.pca,
                          data = datos_varred_re[1:100,],
                          colour = 'RANGOS')

pca.plot

plot(res.pca$PC1,
     res.pca$PC2,
     col = PC$Label,
     pch = 19)
