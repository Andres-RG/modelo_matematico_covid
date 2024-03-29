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
library(vegan)

# Se carga la base de datos
load("03_Out/OutData/casos_positivos_rangos_edades.RData")
nmds1_resultados <- readRDS("03_Out/OutData/nmds1_resultados.rds")
load("03_Out/OutData/df_menores_mayores.RData")

# Se cargan las funciones
source("02_Scripts/Functions/Functions.R")

# ANALISIS DE COMPONENTES PRINCIPALES
# 1. Se reduce la base de datos a solo las columnas necesarias
datos_varred <- select(casos_positivos_re, 
                       c("EDAD","DIABETES", "HIPERTENSION", "CARDIOVASCULAR", 
                         "OBESIDAD", "FECHA_DEF", "TIPO_PACIENTE"))

# 2. Se añaden los rangos de edad pero solamente en terminos numericos
#    1 == Menores de 18 años
#    2 == 18 - 29 años
#    3 == 30 - 39 años
#    4 == 40 - 49 años
#    5 == 50 - 59 años
#    6 == 60 - 69 años
#    7 == Mayores de 70 años
renumeric <- rangos_edades_only_nums(datos_varred$EDAD)
renonumeric <- rangos_edades(datos_varred$EDAD)
# 2.1. Se añade la columna de rangos de edad a la base de datos y se elimina la columna de edad
datos_varred_re <- mutate(datos_varred, "RANGOS" = renonumeric)
datos_varred <- datos_varred_re %>% select(c(-EDAD))
# 2.2. Se añade la columna de muerte y se elimina FECHA_DEF
#      1 <-------- falleció
#      2 <-------- no falleció / recuperado
datos_varred <- mutate(datos_varred,
                       "DEF" = c ( ifelse( !is.na(casos_positivos_re$FECHA_DEF),1, 2) ))
datos_varred <- datos_varred %>% select(c(-FECHA_DEF))
# 2.3 Se añade la columna de hospitalizados y se elimina TIPO_PACIENTE
#      2 <-------- hospitalizado
#      1 <-------- ambulatorio
datos_varred <- mutate(datos_varred,
                     "HOSP" = c ( ifelse (datos_varred$TIPO_PACIENTE == 2, 2, 1)))
datos_varred <- datos_varred %>% select(c(-TIPO_PACIENTE))
# save(datos_varred, file = "03_Out/OutData/datos_positivos_reducidos.RData")
datos_varred2 <- datos_varred[1:5000,]
datos_varred12 <- datos_varred[ , -5]
# 3. PCA
covid_pca <- PCA(datos_varred12, graph = F)

# 4. NMDS 
# 40 entradas ----
d1 <- filter(datos_varred, RANGOS == "18-")
d2 <- filter(datos_varred, RANGOS == "70+")

#m1 <- sample(nrow(d1),20, replace = F)
#m2 <- sample(nrow(d2),20, replace = F)

c1 <- c(d1[m1,])
c2 <- c(d2[m2,])

df1 <- data.frame(d1)
df2 <- data.frame(d2)

combined_df <- rbind(df1, df2)
# save(combined_df, file = "03_Out/OutData/df_menores_mayores.RData")
#nmds estimador de distancia para datos discretos jaccard sorensen ----
#cuanta variacion explica los dos primeros ejes, screeplot, eigenvector para ver si las variables originales explican la mayor variación

# NMDS---
#set.seed(0)#Para que los resultados no se brinden aleatorios
#nmds1 <- metaMDS(combined_df[,-5], distance = "jaccard")
#saveRDS(nmds1, file = "03_Out/OutData/nmds1_resultados.rds",
#        compress = "xz") #GUARDAR OBJETO
nmds1_resultados #---
# stressplot
#jpeg("03_Out/Plots/stressplot_nmds1.jpeg", width = 395, height = 265, res = 300, units = "mm")
stressplot(nmds1_resultados)
#dev.off()
# plot
plot(nmds1_resultados)
ordiplot(nmds1_resultados,type="n")
orditorp(nmds1_resultados,display="species",col="red",air=0.01)
orditorp(nmds1_resultados,display="sites",cex=1.25,air=0.01)
#
coordenadas <- as.data.frame(scores(nmds1_resultados)$sites)
coordenadas
coordenadas$RANGOS = combined_df$RANGOS
head(coordenadas)
gr2<- ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes( shape = RANGOS, colour = RANGOS))+
  geom_text(hjust=0.5, vjust=1.5, label=combined_df$RANGOS)
#jpeg("03_Out/Plots/nmds1.jpeg",width = 365, height = 265, res = 300, units = "mm")
gr2
#dev.off()