# Librerias necesarias 

library(tidyverse)
library(deSolve)
library(ape)
library(lubridate)

# Cargar las funciones
source("02_Scripts/Functions/Functions.R")

# Se carga la base de datos
load("01_RawData/datos_covid_qro.RData")

# 1. Ragos de edad =============================================================

#    De la base de datos inicial, se determinaran los rangos de edad de todos
#    los casos positivos a Covid-19 hasta 17 de octubre, 2021. A partir de esta
#    adicion de datos, se construye la grafica para casos positivos divididos
#    por rangos de edades. Este objeto contendra todos los casos, con la columna
#    de rango de edades, de cada individuo. Se hace uso de una función
#    desarrollada.

# casos_totales_re <- mutate(datos_covid_qro, rango_de_edad = rangos_edades(datos_covid_qro$EDAD))

#    Se guarda el objeto como un objeto .RData
# save(casos_totales_re, file = "03_Out/OutData/casos_totales_rangos_edades.RData")

# 2. Casos Positivos ===========================================================

#    Este objeto contendra los casos unicamente positivos, de acuerdo a la
#    clasificacion reportada.

casos_positivos_re <- filter(casos_totales_re, CLASIFICACION_FINAL == 1 |
                                 CLASIFICACION_FINAL == 2 |
                                 CLASIFICACION_FINAL == 3 )

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_re, file = "03_Out/OutData/casos_positivos_rangos_edades.RData")

# 3. Filtrado: positivo a ambulatorio ==========================================

#    De todos los casos positivos, se toman aquellos cuya clasificacion es de
#    tipo Ambulatorio.

casos_positivos_leves_re <- filter(casos_positivos_re, TIPO_PACIENTE==1)

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_leves_re, file = "03_Out/OutData/casos_positivos_leves_rangos_edades.RData")

# 4. Filtrado: positivo a hospitalizado ========================================

#    De todos los casos positivos, se toman aquellos cuya clasificacion es de
#    tipo Hospitalizado.

casos_positivos_hospitalizados_re <- filter(casos_positivos_re, TIPO_PACIENTE==2)

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_hospitalizados_re, file="03_Out/OutData/casos_positivos_hospitalizados_rangos_edades.RData")

# 5. Filtrado: positivo y hospitalizado a intubado =============================

#    De todos los casos positivos que fueron hospitalizados, se toman aquellos 
#    que fueron registrados como intubados.

casos_positivos_intubados_re <- filter(casos_positivos_hospitalizados_re, INTUBADO==1)

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_intubados_re, file = "03_Out/OutData/casos_positivos_intubados_rangos_edades.RData")

# 6. Positivos intubados que fallecieron =======================================

#    Para esta base, se establece primero una nueva columna con la condicion del
#    paciente, ya sea que haya fallecido o no, debido a que esta no se indica en
#    los datos crudos. Para esto, se establece un ciclo ifelse, donde si no está
#    presente una NA en la columna indicada como FECHA_DEF, de la fecha de 
#    defuncion, que es el unico indicador del estado del paciente; coloque la 
#    condicion, ya sea de muerte, si es que existe una fecha, y no muerte, si es
#    que hay una NA.

casos_positivos_int_m_re <- mutate(casos_positivos_intubados_re,
                                   muerte = c( ifelse( !is.na( 
                                       casos_positivos_intubados_re$FECHA_DEF ), 
                                             "Muerte", "No muerte") ) )

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_int_m_re, file = "03_Out/OutData/casos_positivos_intubados_a_muerte_rangos_edades.RData")

# 7. Filtrado: intubados que fallecieron =======================================

#    De la base de datos de casos_positivos_int_m_re, que indica el estado de
#    vida del paciente y los rangos de edades, se filtraran todos aquellos casos
#    que fallecieron.

casos_positivos_muerte_re <- filter(casos_positivos_int_m_re, muerte == "Muerte")

#    Se guarda el objeto como un objeto .RData
# save(casos_positivos_muerte_re, file = "03_Out/OutData/casos_positivos_muerte_rangos_edades.RData")

# 8. Casos por dia. Incidencia =================================================

pos <- c() # crea un vector vacio

for (i in 1:length(casos_positivos_re$FECHA_SINTOMAS) ) {
    pos <- c(pos, 1) } # por cada uno de los positivos, 
# coloca un 1 en el vector-

casos_positivos_re_conteo <- mutate(casos_positivos_re, positivos = pos) 
# genera una nueva columna en la base de los casos_positivos_re
# la nueva columna la rellena con el vector de 1's creado. Hay un 1 en todos 
# los renglones. 

# Suma todos los positivos de un solo dia por fecha de inicio de sintomas
casos_positivos_re_conteo <- aggregate(positivos~FECHA_SINTOMAS, 
                                       data = casos_positivos_re_conteo,
                                       FUN = sum)

# Genera otra columna en el objeto para agregar el numero de dia
casos_positivos_re_conteo [,3] <- c(1:length(casos_positivos_re_conteo$FECHA_SINTOMAS))
colnames(casos_positivos_re_conteo)[3] <- "num.dia" 
casos_positivos_re_conteo

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_re_conteo, file = "03_Out/OutData/conteo_casos_positivos_rango_edad.RData")

# 9. Casos por dia con rango de edad. Incidencia ===============================

# Primero se establece que cada renglon es equivalente a un individuo
casos_positivos_x_dia_re <- mutate(casos_positivos_re, individuo = 1)

# Con esta funcion, se suman todos los casos positivos por dia por rango de edad
casos_positivos_x_dia_re <- aggregate(casos_positivos_x_dia_re$individuo, 
                                      by = list(casos_positivos_x_dia_re$rango_de_edad,
                                                casos_positivos_x_dia_re$FECHA_SINTOMAS), 
                                      FUN = sum)
colnames(casos_positivos_x_dia_re) <- c("rango_de_edad", 
                                        "FECHA_SINTOMAS", 
                                        "casos_totales")

# Se obtiene una tabla con las personas positivas por dia y por rango de edad. No contempla 
# su estado (hospitalizados, intubados, etc)
casos_positivos_x_dia_re

# Se guarda el objeto como un objeto .RData
# save(casos_positivos_x_dia_re, file = "03_Out/OutData/casos_positivos_x_dia_rango_edad.RData")

# 10. Casos por grupos etarios

casos_x_grupos <- casos_positivos_x_dia_re %>%
    mutate(grupos = case_when(
        rango_de_edad == "18-" ~ "Menores de 18 años",
        rango_de_edad %in% c("18-29", "30-39") ~ "18-39 años",
        rango_de_edad %in% c("40-49", "50-59") ~ "40-59 años",
        rango_de_edad %in% c("60-69", "70+") ~ "Mayores de 60 años",
    )) %>%
    group_by(grupos, FECHA_SINTOMAS) %>%
    summarise(casos_totales = sum(casos_totales))

#     Ordenar los grupos etarios

casos_x_grupos$grupos <- factor(casos_x_grupos$grupos,
                                levels = c("Menores de 18 años",
                                           "18-39 años",
                                           "40-59 años",
                                           "Mayores de 60 años"))

# save(casos_x_grupos, file = "03_Out/OutData/casos_datos_x_grupos.RData")

#     corte a los 398 dias

casos_x_grupos_corte <- casos_x_grupos %>%
    filter(FECHA_SINTOMAS <= as.Date("2021-04-3"))

# save(casos_x_grupos_corte, file = "03_Out/OutData/casos_x_grupos_corte.RData")

# 11. Muertes por grupo etario =================================================

#     De los casos positivos, se agrega la columna que indica la condición del
#     individuo.
casos_muerte <- mutate(casos_positivos_re, muerte = c( 
    ifelse( !is.na( casos_positivos_re$FECHA_DEF ),"Muerte", "No muerte") ) )

#     Se filtran solo los casos que muerieron

casos_muerte <- filter(casos_muerte, muerte == "Muerte")

ind <- c() # crea un vector vacio
for (i in nrow(casos_muerte) ) {
    ind <- c(ind, 1) } # por cada uno de los casos, coloca un 1 en el vector
casos_muerte <- mutate(casos_muerte, casos = ind)

#     Suma todos los positivos de un solo dia por fecha de inicio de sintomas
conteo_casos_muerte <- aggregate(casos~FECHA_DEF+rango_de_edad, 
                                 data = casos_muerte,
                                 FUN = sum)
#     Agrupa por grupo etario
muertes_x_grupos <- conteo_casos_muerte %>%
    mutate(grupos = case_when(
        rango_de_edad == "18-" ~ "Menores de 18 años",
        rango_de_edad %in% c("18-29", "30-39") ~ "18-39 años",
        rango_de_edad %in% c("40-49", "50-59") ~ "40-59 años",
        rango_de_edad %in% c("60-69", "70+") ~ "Mayores de 60 años",
    )) %>%
    group_by(grupos, FECHA_DEF) %>%
    summarise(casos = sum(casos))

#      Ordenar los grupos etarios.
muertes_x_grupos$grupos <- factor(muertes_x_grupos$grupos,
                                  levels = c("Menores de 18 años",
                                             "18-39 años",
                                             "40-59 años",
                                             "Mayores de 60 años"))

# save(muertes_x_grupos, file = "03_Out/OutData/muertes_datos_x_grupos.RData")

#     corte a los 398 dias
muertes_x_grupos_corte <- muertes_x_grupos %>%
    filter(FECHA_DEF <= as.Date("2021-04-3"))

# save(muertes_x_grupos_corte, file = "03_Out/OutData/muertes_x_grupos_corte.RData")

