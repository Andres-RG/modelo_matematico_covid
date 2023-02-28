# 1. Función para establecer los rangos de edades ====
# Toma como argumento una columna de la base de datos
# En este caso siempre va a ser la edad de los casos
rangos_edades <- function (colm) {
  
  # Genera un vector vacío
  re <- c()
  
  # Establece un ciclo for, en donde, por cada variable i en 1 hasta la longitud
  # de la columna:
  for (i in 1:length(colm)) {
    
    # Si la variable i en la columna de edad es mayor o igual a 18 Y menor o 
    # igual a 29 ...
    if (colm[[i]] >= 18 & colm[[i]] <= 29) {
      re <- c(re, "18-29") #... colocará en el vector el rango de edad "18-29"
    } else if (colm[[i]] >= 30 & colm[[i]] <= 39) { # Si la variable i en la 
                                                    # columna de edad es mayor 
                                                    # o igual a 30 Y menor o 
                                                    # igual a 39 ...
      re <- c(re, "30-39") #... colocará en el vector el rango de edad "30-39"
    } else if (colm[[i]] >= 40 & colm[[i]] <= 49) { # Si la variable i en la 
                                                    # columna de edad es mayor 
                                                    # o igual a 40 Y menor o 
                                                    # igual a 49 ...
      re <- c(re, "40-49") #... colocará en el vector el rango de edad "40-49"
    } else if (colm[[i]] >= 50 & colm[[i]] <= 59) { # Si la variable i en la 
                                                    # columna de edad es mayor 
                                                    # o igual a 50 Y menor o 
                                                    # igual a 59 ...
      re <- c(re, "50-59") #... colocará en el vector el rango de edad "50-59"
    } else if (colm[[i]] >= 60 & colm[[i]] <= 69) { # Si la variable i en la 
                                                    # columna de edad es mayor 
                                                    # o igual a 60 Y menor o 
                                                    # igual a 69 ...
      re <- c(re, "60-69") #... colocará en el vector el rango de edad "60-69"
    } else if (colm[[i]] >= 70) { # Si la variable i en la columna de edad es 
                                  # mayor o igual a 70...
      re <- c(re, "70+") #... colocará en el vector el rango de edad "70+"
    } else if (colm[[i]] < 18) { # Si la variable i en la columna de edad es 
                                 # menor a 18...
      re <- c(re, "18-") #... colocará en el vector el rango de edad "18-"
    }
  }
  
  # Convierte el vector con los datos en un objeto tipo vector
  re <- as.vector(re)
  # Regresa el contenido del vector
  return(re)
  
}



# 2. Funcion para determinar las probabilidades de transicion ====
#    Toma como argumento dos bases de datos. Esta funcion sera utilizada para 
#    las bases de datos que ya esten filtradas. # Toma los casos de la base 1 
#    y se dividen entre el total de los casos de la base 2, por rango de edad. 
probabilidades <- function(base_1, base_2){
  
  ##
  p18 <- sum(base_1$rango_de_edad == "18-") / sum(base_2$rango_de_edad == "18-")
  ##
  ##
  p18_29 <- sum(base_1$rango_de_edad == "18-29") / 
    sum(base_2$rango_de_edad == "18-29")
  ##
  ##
  p30_39 <- sum(base_1$rango_de_edad == "30-39") / 
    sum(base_2$rango_de_edad == "30-39")
  ##
  ##
  p40_49 <- sum(base_1$rango_de_edad == "40-49") /
    sum(base_2$rango_de_edad == "40-49")
  ##
  ##
  p50_59 <- sum(base_1$rango_de_edad == "50-59") /
    sum(base_2$rango_de_edad == "50-59")
  ##
  ##
  p60_69 <- sum(base_1$rango_de_edad == "60-69") /
    sum(base_2$rango_de_edad == "60-69")
  ##
  ##
  p70 <- sum(base_1$rango_de_edad == "70+") /
    sum(base_2$rango_de_edad == "70+")
  
  
  # Genera una tabla con todas las probabilidades de todos los rangos de edad
  tabla_prob <- rbind(p18, p18_29, p30_39, p40_49, p50_59, p60_69, p70)
  # Regresa la tabla
  return(tabla_prob)
  
}



# 3. Funcion para establecer las fechas y periodos de vacunacion ====
#   Toma como argumento la fecha, que corresponde a la fecha de inicio de 
#   sintomas de los datos de covid.
fechas_vacunacion <- function(fecha){
  
  # Genera un vector vacio 
  vac <- c()
  
  # Establece un ciclo for, donde va a iterar la variable i en la columna de 
  # las fechas donde: 
  for (i in fecha) {
    
    if( i >= as.Date("2020-12-01") & i <= as.Date("2021-01-31") ) { 
      # Si la fecha esta entre un rango de fechas especificas ...
      vac <- c(vac, "Primera Fase") # ... corresponde a un periodo de vacunacion
                                    # y lo colocara en el vector
    } else if ( i >= as.Date("2021-02-01") & i <= as.Date("2021-04-30") ) {
      vac <- c(vac, "Segunda Fase")
    } else if ( i >= as.Date("2021-05-01") & i <= as.Date("2021-05-31") ) {
      vac <- c(vac, "Tercera Fase")
    } else if ( i >= as.Date("2021-06-01") & i <= as.Date("2021-06-30") ) {
      vac <- c(vac, "Cuarta Fase")
    } else if ( i >= as.Date("2021-07-01") & i <= as.Date("2027-04-07") ) {
      vac <- c(vac, "Quinta Fase")
    } else { # En cambio, todas las fechas que no esten incluidas en los 
             # periodos de vacunacion ... 
      vac <- c(vac, "NA") # Colocara una NA en el vector
    }
  }
  
  # Convierte el vector de las fechas en un objeto tipo vector
  vac <- as.vector(vac)
  # Regresa el objeto
  return(vac)
}



# 4. Funcion para determinar la presencia de comorbilidades ====
#   1 = SI ; 0 = NO 
#   Toma como argumento la base de datos
comorbilidadesdet <- function( base ){
  
  # Genera un vector vacio
  comorbilidades <- c()
  # Genera un vector con una secuencia de 1 hasta el numero de columnas de la 
  # base de datos
  n <- seq(1:ncol(base))
  # Establece un ciclo for, donde para cada variable i en el vector de numeros 
  # de la secuencia N:
  for (i in n) {
    if ( any(base[,i] == 1)){
      # Si hay alguna variable, que sea exactamente igual a 1 en la columna i, 
      # que corresponde a cada caso, indicaria la presencia de esa comorbilidad
      comorbilidades <- c(comorbilidades,1) # coloca entonces, un 1 en el vector
    } else { # En cambio ...
      comorbilidades <- c(comorbilidades,0) # coloca un 0 en el vector
    }
  }
  #Regresa el vector
  return(comorbilidades)
}



# 5. Funcion para el conteo de comorbilidades de cada caso ====
#    Toma como argmuento la base de datos de covid. 
comorbilidades_conteo <- function(base){
  
  # Genera los contadores vacios para cada una de las comorbilidades, 
  # con valor de 0
  diabetes_conteo         <- 0
  epoc_conteo             <- 0
  asma_conteo             <- 0
  inmunsupr_conteo        <- 0
  hipertension_conteo     <- 0
  cardiovascular_conteo   <- 0
  obesidad_conteo         <- 0
  renal_cronica_conteo    <- 0
  tabaquismo_conteo       <- 0
  
  # Establece un ciclo for, donde va a iterar las columnas de la base de datos 
  # con la variable i:
  for (i in colnames(base[,5:13])){ 
    if (i == "DIABETES"){ # Si la variable i es exactamente igual a 
                          # a la comorbilidad especifica ...
      for (n in base[[i]]){ # Establece un segundo ciclo for, donde para cada 
                            # valor de la columna de la comorbilidad:
        if (n != 2){ # Si el valor es diferente a 2 ....
          diabetes_conteo <- diabetes_conteo + 1 # Al contador de la 
                                                 # comorbilidad en especifico, 
                                                 # le suma 1.
        }
      } # Cierra el ciclo cuando ya recorrio toda la columna 
    } else if( i == "EPOC"){ # En cambio, pasa a la siguiente comorbilidad
      for (n in base[[i]]){
        if (n != 2){
          epoc_conteo <- epoc_conteo + 1
        }
      }
    } else if( i == "ASMA"){
      for (n in base[[i]]){
        if (n != 2){
          asma_conteo <- asma_conteo + 1
        }
      }
    } else if( i == "INMUSUPR"){
      for (n in base[[i]]){
        if (n != 2){
          inmunsupr_conteo <- inmunsupr_conteo + 1
        }
      }
    } else if( i == "HIPERTENSION"){
      for (n in base[[i]]){
        if (n != 2){
          hipertension_conteo <- hipertension_conteo + 1
        }
      }
    } else if( i == "CARDIOVASCULAR"){
      for (n in base[[i]]){
        if (n != 2){
          cardiovascular_conteo <- cardiovascular_conteo + 1
        }
      }
    } else if( i == "OBESIDAD"){
      for (n in base[[i]]){
        if (n != 2){
          obesidad_conteo <- obesidad_conteo + 1
        }
      }
    } else if( i == "RENAL_CRONICA"){
      for (n in base[[i]]){
        if (n != 2){
          renal_cronica_conteo <- renal_cronica_conteo + 1
        }
      }
    } else if( i == "TABAQUISMO"){
      for (n in base[[i]]){
        if (n != 2){
          tabaquismo_conteo <- tabaquismo_conteo + 1
        }
      }
    } 
  }
  
  # Genera un vector con todos los contadores una vez recorrio toda la base
  # de datos
  com_conteos <- c(diabetes_conteo         ,
                   epoc_conteo             ,
                   asma_conteo             ,
                   inmunsupr_conteo        ,
                   hipertension_conteo     ,
                   cardiovascular_conteo   ,
                   obesidad_conteo         ,
                   renal_cronica_conteo    ,
                   tabaquismo_conteo       )
  
  
}



# 6. Funcion para el conteo de casos que *NO TIENEN* comorbilidades  ====
#    Se basa en la misma estructura que la funcion anterior, solo que para este 
#    caso, el condicional IF que define la presencia de comorbilidad, es que sea
#    exactamente igual a 2, es decir, de acuerdo a los datos de covid, que sea
#    NO
sin_comorbilidades_conteo <- function(base){
  
  #contadores vacios
  sin_diabetes_conteo         <- 0
  sin_epoc_conteo             <- 0
  sin_asma_conteo             <- 0
  sin_inmunsupr_conteo        <- 0
  sin_hipertension_conteo     <- 0
  sin_cardiovascular_conteo   <- 0
  sin_obesidad_conteo         <- 0
  sin_renal_cronica_conteo    <- 0
  sin_tabaquismo_conteo       <- 0
  
  # para cada columna de la base
  for (i in colnames(base[,5:13])){
    if (i == "DIABETES"){
      for (n in base[[i]]){
        if (n == 2){
          sin_diabetes_conteo <- sin_diabetes_conteo + 1
        }
      }
    } else if( i == "EPOC"){
      for (n in base[[i]]){
        if (n == 2){
          sin_epoc_conteo <- sin_epoc_conteo + 1
        }
      }
    } else if( i == "ASMA"){
      for (n in base[[i]]){
        if (n == 2){
          sin_asma_conteo <- sin_asma_conteo + 1
        }
      }
    } else if( i == "INMUSUPR"){
      for (n in base[[i]]){
        if (n == 2){
          sin_inmunsupr_conteo <- sin_inmunsupr_conteo + 1
        }
      }
    } else if( i == "HIPERTENSION"){
      for (n in base[[i]]){
        if (n == 2){
          sin_hipertension_conteo <- sin_hipertension_conteo + 1
        }
      }
    } else if( i == "CARDIOVASCULAR"){
      for (n in base[[i]]){
        if (n == 2){
          sin_cardiovascular_conteo <- sin_cardiovascular_conteo + 1
        }
      }
    } else if( i == "OBESIDAD"){
      for (n in base[[i]]){
        if (n == 2){
          sin_obesidad_conteo <- sin_obesidad_conteo + 1
        }
      }
    } else if( i == "RENAL_CRONICA"){
      for (n in base[[i]]){
        if (n == 2){
          sin_renal_cronica_conteo <- sin_renal_cronica_conteo + 1
        }
      }
    } else if( i == "TABAQUISMO"){
      for (n in base[[i]]){
        if (n == 2){
          sin_tabaquismo_conteo <- sin_tabaquismo_conteo + 1
        }
      }
    } 
  }
  
  sin_com_conteos <- c(sin_diabetes_conteo         ,
                       sin_epoc_conteo             ,
                       sin_asma_conteo             ,
                       sin_inmunsupr_conteo        ,
                       sin_hipertension_conteo     ,
                       sin_cardiovascular_conteo   ,
                       sin_obesidad_conteo         ,
                       sin_renal_cronica_conteo    ,
                       sin_tabaquismo_conteo       )
  
  
}

# 7. Funcion para hacer los conteos de casos con comorbilidades con combinaciones----
comorbilidades_combinadas_conteo <- function(base) {
  
  # CONTADORES
  diabetes_epoc <- 0
  diabetes_asma <- 0
  diabetes_inmunsupr <- 0
  diabetes_hipertension <- 0
  diabetes_cardiovascular <- 0
  diabetes_obesidad <- 0
  diabetes_renal_cronica <- 0
  diabetes_tabaquismo <- 0
  
  epoc_asma <- 0
  epoc_inmunsupr <- 0
  epoc_hipertension <- 0
  epoc_cardiovascular <- 0
  epoc_obesidad <- 0
  epoc_renal_cronica <- 0
  epoc_tabaquismo <- 0
  
  asma_inmunsupr <- 0
  asma_hipertension <- 0
  asma_cardiovascular <- 0
  asma_obesidad <- 0
  asma_renal_cronica <- 0
  asma_tabaquismo <- 0
  
  inmunsupr_hipertension <- 0
  inmunsupr_cardiovascular <- 0
  inmunsupr_obesidad <- 0
  inmunsupr_renal_cronica <- 0
  inmunsupr_tabaquismo <- 0
  
  hipertension_cardiovascular <- 0
  hipertension_obesidad <- 0
  hipertension_renal_cronica <- 0
  hipertension_tabaquismo <- 0
  
  cardiovascular_obesidad <- 0
  cardiovascular_renal_cronica <- 0
  cardiovascular_tabaquismo <- 0
  
  obesidad_renal_cronica <- 0
  obesidad_tabaquismo <- 0
  
  renal_cronica_tabaquismo <- 0
  
  
  for ( i in colnames(base) ) {
    if (i == "DIABETES") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if (i == "EPOC"){
              for (n in base[[i]] ){
                if (n != 2){
                  diabetes_epoc <- diabetes_epoc + 1
                }
              }
            } else if(i == "ASMA") {
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_asma <- diabetes_asma + 1
                }
              }
            } else if(i == "INMUSUPR") {
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_inmunsupr <- diabetes_inmunsupr + 1
                }
              }
            } else if(i == "HIPERTENSION"){
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_hipertension <- diabetes_hipertension + 1
                }
              }
            } else if(i == "CARDIOVASCULAR"){
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_cardiovascular <- diabetes_cardiovascular + 1
                }
              }
            } else if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_obesidad <- diabetes_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_renal_cronica <- diabetes_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  diabetes_tabaquismo <- diabetes_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "EPOC") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            
            if(i == "ASMA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_asma <- epoc_asma + 1
                }
              }
            } else if(i == "INMUSUPR") {
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_inmunsupr <- epoc_inmunsupr + 1
                }
              }
            } else if(i == "HIPERTENSION"){
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_hipertension <- epoc_hipertension + 1
                }
              }
            } else if(i == "CARDIOVASCULAR"){
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_cardiovascular <- epoc_cardiovascular + 1
                }
              }
            } else if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_obesidad <- epoc_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_renal_cronica <- epoc_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  epoc_tabaquismo <- epoc_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "ASMA") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if(i == "INMUSUPR") {
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_inmunsupr <- asma_inmunsupr + 1
                }
              }
            } else if(i == "HIPERTENSION"){
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_hipertension <- asma_hipertension + 1
                }
              }
            } else if(i == "CARDIOVASCULAR"){
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_cardiovascular <- asma_cardiovascular + 1
                }
              }
            } else if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_obesidad <- asma_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_renal_cronica <- asma_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  asma_tabaquismo <- asma_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "INMUSUPR") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if(i == "HIPERTENSION"){
              for (n in base[[i]] ){
                if (n != 2) {
                  inmunsupr_hipertension <- inmunsupr_hipertension + 1
                }
              }
            } else if(i == "CARDIOVASCULAR"){
              for (n in base[[i]] ){
                if (n != 2) {
                  inmunsupr_cardiovascular <- inmunsupr_cardiovascular + 1
                }
              }
            } else if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  inmunsupr_obesidad <- inmunsupr_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  inmunsupr_renal_cronica <- inmunsupr_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  inmunsupr_tabaquismo <- inmunsupr_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "HIPERTENSION") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if(i == "CARDIOVASCULAR"){
              for (n in base[[i]] ){
                if (n != 2) {
                  hipertension_cardiovascular <- hipertension_cardiovascular + 1
                }
              }
            } else if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  hipertension_obesidad <- hipertension_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  hipertension_renal_cronica <- hipertension_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  hipertension_tabaquismo <- hipertension_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "CARDIOVASCULAR") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if (i == "OBESIDAD") {
              for (n in base[[i]] ){
                if (n != 2) {
                  cardiovascular_obesidad <- cardiovascular_obesidad + 1
                }
              }
            } else if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  cardiovascular_renal_cronica <- cardiovascular_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  cardiovascular_tabaquismo <- cardiovascular_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "OBESIDAD") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if (i == "RENAL_CRONICA"){
              for (n in base[[i]] ){
                if (n != 2) {
                  obesidad_renal_cronica <- obesidad_renal_cronica + 1
                }
              }
            } else if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  obesidad_tabaquismo <- obesidad_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    } else if (i == "RENAL_CRONICA") {
      for ( n in base[[i]] ) {
        if (n != 2) {
          
          for ( i in colnames(base) ){
            
            if (i == "TABAQUISMO"){
              for (n in base[[i]] ){
                if (n != 2) {
                  renal_cronica_tabaquismo <- renal_cronica_tabaquismo + 1
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  conteo <- c(NA,diabetes_epoc,diabetes_asma,diabetes_inmunsupr, 
              diabetes_hipertension, diabetes_cardiovascular,
              diabetes_obesidad, diabetes_renal_cronica,diabetes_tabaquismo,
              
              0,NA,epoc_asma,epoc_inmunsupr,epoc_hipertension,epoc_cardiovascular,
              epoc_obesidad,epoc_renal_cronica,epoc_tabaquismo,
              
              0,0,NA,asma_inmunsupr,asma_hipertension,asma_cardiovascular,asma_obesidad,
              asma_renal_cronica,asma_tabaquismo,
              
              0,0,0,NA,inmunsupr_hipertension,inmunsupr_cardiovascular,
              inmunsupr_obesidad,inmunsupr_renal_cronica,inmunsupr_tabaquismo,
              
              0,0,0,0,NA,hipertension_cardiovascular,hipertension_obesidad,
              hipertension_renal_cronica,hipertension_tabaquismo,
              
              0,0,0,0,0,NA,cardiovascular_obesidad,cardiovascular_renal_cronica,
              cardiovascular_tabaquismo,
              
              0,0,0,0,0,0,NA,obesidad_renal_cronica,obesidad_tabaquismo,
              
              0,0,0,0,0,0,0,NA,renal_cronica_tabaquismo,
              
              0,0,0,0,0,0,0,0,NA)
  
  matriz_conteos <- matrix(conteo, nrow = 9, ncol = 9,byrow = F)
  colnames(matriz_conteos) <- c("Diabetes","EPOC","Asma","Inmunsupr",
                                "Hipertension","Cardiovascular","Obesidad",
                                "Renal Cronica","Tabaquismo")
  rownames(matriz_conteos) <- c("Diabetes","EPOC","Asma","Inmunsupr",
                                "Hipertension","Cardiovascular","Obesidad",
                                "Renal Cronica","Tabaquismo")
  return(matriz_conteos)
  
}