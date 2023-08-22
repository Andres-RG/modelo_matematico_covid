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
        if (n == 1){ # Si el valor es diferente a 2 ....
          diabetes_conteo <- diabetes_conteo + 1 # Al contador de la 
                                                 # comorbilidad en especifico, 
                                                 # le suma 1.
        }
      } # Cierra el ciclo cuando ya recorrio toda la columna 
    } else if( i == "EPOC"){ # En cambio, pasa a la siguiente comorbilidad
      for (n in base[[i]]){
        if (n == 1){
          epoc_conteo <- epoc_conteo + 1
        }
      }
    } else if( i == "ASMA"){
      for (n in base[[i]]){
        if (n == 1){
          asma_conteo <- asma_conteo + 1
        }
      }
    } else if( i == "INMUSUPR"){
      for (n in base[[i]]){
        if (n == 1){
          inmunsupr_conteo <- inmunsupr_conteo + 1
        }
      }
    } else if( i == "HIPERTENSION"){
      for (n in base[[i]]){
        if (n == 1){
          hipertension_conteo <- hipertension_conteo + 1
        }
      }
    } else if( i == "CARDIOVASCULAR"){
      for (n in base[[i]]){
        if (n == 1){
          cardiovascular_conteo <- cardiovascular_conteo + 1
        }
      }
    } else if( i == "OBESIDAD"){
      for (n in base[[i]]){
        if (n == 1){
          obesidad_conteo <- obesidad_conteo + 1
        }
      }
    } else if( i == "RENAL_CRONICA"){
      for (n in base[[i]]){
        if (n == 1){
          renal_cronica_conteo <- renal_cronica_conteo + 1
        }
      }
    } else if( i == "TABAQUISMO"){
      for (n in base[[i]]){
        if (n == 1){
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
#    Toma como argumento una base de datos, que deberia estar filtrada por los
#    rangos de edades.
comorbilidades_combinadas_conteo <- function(base) {
  
  # Cuenta con las condicionales para cada renglon. Si en la base de datos, en 
  # las columnas especificadas, los valores son iguales, los cuenta.
  # Realiza el mismo proceso para cada combinacion
  diabetes_epoc <- nrow(base[base$DIABETES == 1 & base$EPOC == 1,])
  diabetes_asma <- nrow(base[base$DIABETES == 1 & base$ASMA == 1,])
  diabetes_inmunsupr <- nrow(base[base$DIABETES == 1 & base$INMUSUPR == 1,])
  diabetes_hipertension <- nrow(base[base$DIABETES == 1 & base$HIPERTENSION == 1,])
  diabetes_cardiovascular <- nrow(base[base$DIABETES == 1 & base$CARDIOVASCULAR == 1,])
  diabetes_obesidad <- nrow(base[base$DIABETES == 1 & base$OBESIDAD == 1,])
  diabetes_renal_cronica <- nrow(base[base$DIABETES == 1 & base$RENAL_CRONICA == 1,])
  diabetes_tabaquismo <- nrow(base[base$DIABETES == 1 & base$TABAQUISMO == 1,])
  
  epoc_asma <- nrow(base[base$EPOC == 1 & base$ASMA == 1,])
  epoc_inmunsupr <- nrow(base[base$EPOC == 1 & base$INMUSUPR == 1,])
  epoc_hipertension <- nrow(base[base$EPOC == 1 & base$HIPERTENSION == 1,])
  epoc_cardiovascular <- nrow(base[base$EPOC == 1 & base$CARDIOVASCULAR == 1,])
  epoc_obesidad <- nrow(base[base$EPOC == 1 & base$OBESIDAD == 1,])
  epoc_renal_cronica <- nrow(base[base$EPOC == 1 & base$RENAL_CRONICA == 1,])
  epoc_tabaquismo <- nrow(base[base$EPOC == 1 & base$TABAQUISMO == 1,])
  
  asma_inmunsupr <- nrow(base[base$ASMA == 1 & base$INMUSUPR == 1,])
  asma_hipertension <- nrow(base[base$ASMA == 1 & base$HIPERTENSION == 1,])
  asma_cardiovascular <- nrow(base[base$ASMA == 1 & base$CARDIOVASCULAR == 1,])
  asma_obesidad <- nrow(base[base$ASMA == 1 & base$OBESIDAD == 1,])
  asma_renal_cronica <- nrow(base[base$ASMA == 1 & base$RENAL_CRONICA == 1,])
  asma_tabaquismo <- nrow(base[base$ASMA == 1 & base$TABAQUISMO == 1,])
  
  inmunsupr_hipertension <- nrow(base[base$INMUSUPR == 1 & base$HIPERTENSION == 1,])
  inmunsupr_cardiovascular <- nrow(base[base$INMUSUPR == 1 & base$CARDIOVASCULAR== 1,])
  inmunsupr_obesidad <- nrow(base[base$INMUSUPR == 1 & base$OBESIDAD == 1,])
  inmunsupr_renal_cronica <- nrow(base[base$INMUSUPR == 1 & base$RENAL_CRONICA == 1,])
  inmunsupr_tabaquismo <- nrow(base[base$INMUSUPR == 1 & base$TABAQUISMO == 1,])
  
  hipertension_cardiovascular <- nrow(base[base$HIPERTENSION == 1 & base$CARDIOVASCULAR == 1,])
  hipertension_obesidad <- nrow(base[base$HIPERTENSION == 1 & base$OBESIDAD == 1,])
  hipertension_renal_cronica <- nrow(base[base$HIPERTENSION == 1 & base$RENAL_CRONICA == 1,])
  hipertension_tabaquismo <- nrow(base[base$HIPERTENSION == 1 & base$TABAQUISMO == 1,])
  
  cardiovascular_obesidad <- nrow(base[base$CARDIOVASCULAR == 1 & base$OBESIDAD == 1,])
  cardiovascular_renal_cronica <- nrow(base[base$CARDIOVASCULAR == 1 & base$RENAL_CRONICA == 1,])
  cardiovascular_tabaquismo <- nrow(base[base$CARDIOVASCULAR == 1 & base$TABAQUISMO== 1,])
  
  obesidad_renal_cronica <- nrow(base[base$OBESIDAD == 1 & base$RENAL_CRONICA == 1,])
  obesidad_tabaquismo <- nrow(base[base$OBESIDAD == 1 & base$TABAQUISMO == 1,])
  
  renal_cronica_tabaquismo <- nrow(base[base$RENAL_CRONICA == 1 & base$TABAQUISMO == 1,])
  
  conteo <- c(
    ### DIABETES
    NA,diabetes_epoc,diabetes_asma,diabetes_inmunsupr,diabetes_hipertension,
    diabetes_cardiovascular,diabetes_obesidad,diabetes_renal_cronica,
    diabetes_tabaquismo,
    
    ### EPOC
    diabetes_epoc,NA,epoc_asma,epoc_inmunsupr,epoc_hipertension,
    epoc_cardiovascular,epoc_obesidad,epoc_renal_cronica,epoc_tabaquismo,
    
    ### ASMA
    diabetes_asma,epoc_asma,NA,asma_inmunsupr,asma_hipertension,
    asma_cardiovascular,asma_obesidad,asma_renal_cronica,asma_tabaquismo,
    
    ### INMUNSUPR
    diabetes_inmunsupr,epoc_inmunsupr,asma_inmunsupr,NA,
    inmunsupr_hipertension,inmunsupr_cardiovascular,inmunsupr_obesidad,
    inmunsupr_renal_cronica,inmunsupr_tabaquismo,
    
    ### HIPERTENSION
    diabetes_hipertension,epoc_hipertension,asma_hipertension,
    inmunsupr_hipertension,NA,hipertension_cardiovascular,
    hipertension_obesidad, hipertension_renal_cronica,
    hipertension_tabaquismo,
    
    ### CARDIOVASCULAR
    diabetes_cardiovascular,epoc_cardiovascular,asma_cardiovascular,
    inmunsupr_cardiovascular,hipertension_cardiovascular,NA,
    cardiovascular_obesidad,cardiovascular_renal_cronica,
    cardiovascular_tabaquismo,
    
    ### OBESIDAD
    diabetes_obesidad,epoc_obesidad,asma_obesidad,inmunsupr_obesidad,
    hipertension_obesidad,cardiovascular_obesidad,NA,
    obesidad_renal_cronica,obesidad_tabaquismo,
    
    ### RENAL CRONICA
    diabetes_renal_cronica,epoc_renal_cronica,asma_renal_cronica,
    inmunsupr_renal_cronica,hipertension_renal_cronica,
    cardiovascular_renal_cronica,obesidad_renal_cronica,NA,
    renal_cronica_tabaquismo,
    
    ### TABAQUISMO
    diabetes_tabaquismo,epoc_tabaquismo,asma_tabaquismo,
    inmunsupr_tabaquismo,hipertension_tabaquismo,cardiovascular_tabaquismo,
    obesidad_tabaquismo,renal_cronica_tabaquismo,NA)
  
  matriz_conteos <- matrix(conteo, nrow = 9, ncol = 9, byrow = F)
  
  colnames(matriz_conteos) <- c("Diabetes","EPOC","Asma","Inmunsupr",
                                "Hipertension","Cardiovascular","Obesidad",
                                "Renal Cronica","Tabaquismo")
  rownames(matriz_conteos) <- c("Diabetes","EPOC","Asma","Inmunsupr",
                                "Hipertension","Cardiovascular","Obesidad",
                                "Renal Cronica","Tabaquismo")
  return(matriz_conteos)
  
}

# 8 Determinar p(com & com & cat) ----
probabilidades_combinaciones <- function( b, m){
  
  # Sumar todos los individuos de la categoria
  categ <- sum(b$ind)
  
  # Crear una matriz de 9x9
  probs <- matrix(nrow = 9, ncol = 9)
  
  # Asignar nombres
  colnames(probs) <- c("diabetes"         ,
                       "epoc"             ,
                       "asma"             ,
                       "inmunsupr"        ,
                       "hipertension"     ,
                       "cardiovascular"   ,
                       "obesidad"         ,
                       "renal_cronica"    ,
                       "tabaquismo"       )
  
  rownames(probs) <- c("diabetes"         ,
                       "epoc"             ,
                       "asma"             ,
                       "inmunsupr"        ,
                       "hipertension"     ,
                       "cardiovascular"   ,
                       "obesidad"         ,
                       "renal_cronica"    ,
                       "tabaquismo"       )
  
  # Ciclo FOR
  for (i in 1:ncol(m)) {
    for (j in 1:nrow(m)) {
      
      probs[i,j] <- round(m[i,j] / categ,5)
      
    }
  }
  
  # Devolver el resultado
  return(probs)
}
# 9. Funcion para el conteo de casos que tiene al menos UNA comorbilidad ------
#    Toma como argumentos la base de datos de cada categoria. En la funcion 
#    estan definidas las columnas 3:13, que son donde estan contenidas las
#    comorbilidades
conteo_una_comorbilidad <- function(base){
  # Se genera un contador vacio
  c <- 0
  # Se genera un ciclo for() para que itere en todos los renglones
  for(i in 1:nrow(base)){
    # Se ponde la funcion logica en ek ciclo if
    if (any(base[i,3:13] == 1)){
      # Se suma 1 al contador si la afirmacion es positiva
      c <- c + 1
    }
  }
  # Devuelve c
  return(c)
}

# 10. Funcion para establecer los rangos de edad en valores numericos estrictamente
rangos_edades_only_nums <- function (colm) {
  
  # Genera un vector vacío
  re <- c()
  
  # Establece un ciclo for, en donde, por cada variable i en 1 hasta la longitud
  # de la columna:
  for (i in 1:length(colm)) {
    
    # Si la variable i en la columna de edad es mayor o igual a 18 Y menor o 
    # igual a 29 ...
    if (colm[[i]] >= 18 & colm[[i]] <= 29) {
      re <- c(re, 2) #... colocará en el vector el rango de edad "18-29"
    } else if (colm[[i]] >= 30 & colm[[i]] <= 39) { # Si la variable i en la 
      # columna de edad es mayor 
      # o igual a 30 Y menor o 
      # igual a 39 ...
      re <- c(re, 3) #... colocará en el vector el rango de edad "30-39"
    } else if (colm[[i]] >= 40 & colm[[i]] <= 49) { # Si la variable i en la 
      # columna de edad es mayor 
      # o igual a 40 Y menor o 
      # igual a 49 ...
      re <- c(re, 4) #... colocará en el vector el rango de edad "40-49"
    } else if (colm[[i]] >= 50 & colm[[i]] <= 59) { # Si la variable i en la 
      # columna de edad es mayor 
      # o igual a 50 Y menor o 
      # igual a 59 ...
      re <- c(re, 5) #... colocará en el vector el rango de edad "50-59"
    } else if (colm[[i]] >= 60 & colm[[i]] <= 69) { # Si la variable i en la 
      # columna de edad es mayor 
      # o igual a 60 Y menor o 
      # igual a 69 ...
      re <- c(re, 6) #... colocará en el vector el rango de edad "60-69"
    } else if (colm[[i]] >= 70) { # Si la variable i en la columna de edad es 
      # mayor o igual a 70...
      re <- c(re, 7) #... colocará en el vector el rango de edad "70+"
    } else if (colm[[i]] < 18) { # Si la variable i en la columna de edad es 
      # menor a 18...
      re <- c(re, 1) #... colocará en el vector el rango de edad "18-"
    }
  }
  
  # Convierte el vector con los datos en un objeto tipo vector
  re <- as.vector(re)
  # Regresa el contenido del vector
  return(re)
  
}