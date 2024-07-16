#****************************************************
#*
#* SISTEMA DE CRECIMIENTO DE Pinus tadea L.
#* Autores: Gyawali Nabin & Burkhart Harold E. (2014)
#* 
#****************************************************


# Modelo de Área Basal

fn_AB2 <- function(AB1, E1, E2, Et){
  
  # Parámetros del modelo de AB
  B1 = 0.1488;  B2 = 0.2978;
  
  # Parámetros de la función de respuesta de aclareo
  r = 0.1190; k = 30.8038
  
  # Función de respuesta del aclareo
  f_thin = (1/B)^(r*(-(E2-Et)^2 + k(E2-Et)) / E2^2)
  
  # Modelo de proyección (Tipo Chapman-Richards)
  AB2 = AB1 * ( (1-exp(-B1*E2)) / (1-exp(-B1*E2))) * f_thin
  
  return(AB2)
  
}