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