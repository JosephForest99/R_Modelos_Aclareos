# Modelos de Mortalidad ---------------------------------------------------

## González-Benecke et al (2012)
N2_fn1 <- function(B1, B2, AD1, N1, E1, E2){
  
  N2 = N1 * exp(B1*AD1*(E2^B2-E1^B2))
  
  return(N2)
}

nls(N2 ~ N2_fn1(B1, B2 = 1, AD, N, E, E2),
    data = datos_notras,
    start = list(B1 = -0.0015))

# Adan Nava (OK)
N2_fn2 <- function(B1, N1, E1, E2){
  
  N2 = N1 * exp(B1*(E2-E1))
  
  return(N2)
  
}

nls(N2 ~ N2_fn2(B1, N, E, E2),
      data = datos_notras,
      start = c(B1 = -0.012))

