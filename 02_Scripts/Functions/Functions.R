# Función para establecer los rangos de edades

rangos_edades <- function (colm) {
  
  re <- c()
  
  for (i in 1:length(colm)) {
    
    if (colm[[i]] >= 18 & colm[[i]] <= 29) {
      re <- c(re, "18-29")
    } else if (colm[[i]] >= 30 & colm[[i]] <= 39) {
      re <- c(re, "30-39")
    } else if (colm[[i]] >= 40 & colm[[i]] <= 49) {
      re <- c(re, "40-49")
    } else if (colm[[i]] >= 50 & colm[[i]] <= 59) {
      re <- c(re, "50-59")
    } else if (colm[[i]] >= 60 & colm[[i]] <= 69) {
      re <- c(re, "60-69")
    } else if (colm[[i]] >= 70) {
      re <- c(re, "70+")
    } else if (colm[[i]] < 18) {
      re <- c(re, "18-")
    }
  }
  
  re <- as.vector(re)
  
  return(re)
  
}










# Funcion para determinar las probabilidades de transicion

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
  
  
  
  tabla_prob <- rbind(p18, p18_29, p30_39, p40_49, p50_59, p60_69, p70)
  
  return(tabla_prob)
  
}










# Funcion para establecer las fechas y periodos de vacunacion

fechas_vacunacion <- function(fecha){
  
  vac <- c()
  
  for (i in fecha) {
    
    if( i >= as.Date("2020-12-01") & i <= as.Date("2021-01-31") ) {
      vac <- c(vac, "Primera Fase")
    } else if ( i >= as.Date("2021-02-01") & i <= as.Date("2021-04-30") ) {
      vac <- c(vac, "Segunda Fase")
    } else if ( i >= as.Date("2021-05-01") & i <= as.Date("2021-05-31") ) {
      vac <- c(vac, "Tercera Fase")
    } else if ( i >= as.Date("2021-06-01") & i <= as.Date("2021-06-30") ) {
      vac <- c(vac, "Cuarta Fase")
    } else if ( i >= as.Date("2021-07-01") & i <= as.Date("2027-04-07") ) {
      vac <- c(vac, "Quinta Fase")
    } else {
      vac <- c(vac, "NA")
    }
  }
  
  vac <- as.vector(vac)
  
  return(vac)
}










# Funcion para determinar la presencia de comorbilidades 
# 1 = SI ; 0 = NO 
comorbilidadesdet <- function( base ){
  comorbilidades <- c()
  ##
  n <- seq(1:ncol(base))
  for (i in n) {
    if ( any(base[,i] == 1)){
      comorbilidades <- c(comorbilidades,1)
    } else {
      comorbilidades <- c(comorbilidades,0)
    }
  }
  ##
  return(comorbilidades)
}
