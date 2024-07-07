
# Sistema de Crecimiento y Rendimiento Sin Aclareo ------------------------

SCR_SA <- function(E, IS, Eb, Narb){
  
  # Modelo de mortalidad
  mortalidad <- function(Narb, E){
    
    # Parámetros 
    b1 = -0.075542
    N <- NA
    N[1] <- Narb
    
    for (i in 2:length(E)) {
      
      N[i] <- N[i-1] * exp(b1*(E[i] - E[i-1]))
      
    }
    
    return(round(N,0))
    
  }
 
  # Modelo de Altura Dominante
  AD <- function(AD1, E1, E2){
    
    # Parámetros
    B1 = 0.05341; B2 = 1.04921
    
    AD2 = AD1 * ((1 - exp(-B1 * E2))/(1 - exp(-B1 * E1)))^B2
    
    return(AD2)
  }
  
  # Modelo de Área Basal
  AB <- function(N, E, AD){
    
    # Parámetros con SUR
    # B0 = 0.05734241; B1 = 0.49064962; B2 = 0.38534127; B3 = 0.69623634
    
    # Parámetros con nls
    B0 = 0.08798 ; B1 = 0.43863 ; B2 = 0.34107 ; B3 = 0.71335 
    
    AB = B0*N^B1*E^B2*AD^B3
    # AB2 = AB1 * (N2/N1)^B1 * (E2/E1)^B2 * (AD2/AD1)^B3
    
    return(AB)
  
  }
  
  # Aplicación de modelos
  N2 <- mortalidad(Narb = Narb, E) 
  # Altura dominante
  AD2 <- AD(AD1 = IS, E2 = E, E1 = Eb)
  # Area basal
  AB2 <- AB(N = N2, E = E, AD = AD2)
  
  SCR <- data.frame(E, N2, AD2, AB2)
  
  return(SCR)
  
}

SCR_SA(E = 0:40, IS = 20, Eb = 30, Narb = 1200)
  