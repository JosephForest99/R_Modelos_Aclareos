
# SCR de González-Benecke et al (2012) ------------------------------------
# Ejemplo con resultados del artículo de González-Benecke et al (2012)


## Modelo de AD ----

AD2_GB2012 <- function(AD1, E, EB){
  B1 = -0.0369815
  B2 = 1.2928702
  
  AD2 = AD1 * ((1-exp(B1*E))/(1-exp(B1*EB)))^B2
  return(AD2)
}

# Aplicación
E = 1:70
IS = 20
EB = 50

AD2 <- AD2_GB2012(IS, E, EB)

plot(E, AD2, type = "l")


## Modelo de N ----

N2_GB2012 <- function(N1, AD2, E){
  B1 = -0.0015002
  B2 = 0.8635401
  
  N2 = NA
  N2[1] = N1*0.95
  
  
  for (i in 2:length(E)) {
    
    N2[i] <- N2[i-1]*exp((B1*AD2) * (E[i]^B2-E[i-1]^B2))
    
  }
  
  return(N2)
}

# Aplicación
N2 <- N2_GB2012(1400, AD2, E)

plot(E, N2, type = "l")


## Modelo de AB ----

BA2_GB2012 <- function(N2, AD2){
  B1 = -4.6484039
  B2 = 0.4452486
  B3 = 1.6526307
  
  AB2 = exp(B1+B2*log(N2)+B3*log(AD2))
  return(AB2)
}

# Aplicación
AB2 = BA2_GB2012(N2, AD2)

plot(E, AB2)


## Modelo de CI ----

CI2_GB2012 <- function(CI1, E1, E2){
  B1 = -1.5476196
  
  CI2 = CI1*exp((B1/E2) * (E2-E1))
  return(CI2)
}

CI2_GB2012(0.001, 10, 11)

## UN EJEMPLO DE APLICACIÓN ----

CI = c(rep(0, 29), 0.6, seq(0.6, 0.01, length.out = 40))


AB2_A <- AB2*(1-CI)

plot(E, AB2_A)
plot(sort(c(E,30)), AB2_A)

















