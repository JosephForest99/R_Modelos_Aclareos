## Gónzalez-Benecke et al 2012 (modificado) ----
fn_AB_00 <- function(B0, B1, B2, B3, N, E, AD){
  
  AB = B0*N^B1*E^B2*AD^B3
  
  return(AB)
  
}

# Ajuste del modelo
fit_AB_00 <- nls(AB ~ fn_AB_00(B0, B1, B2, B3, N, E, AD),
              data = datos,
              start = c(B0 = 1, B1 = 0.5, B2 = 1, B3 = 0.9))

# Resumen del ajuste
summary(fit_AB_00)

## Gónzalez-Benecke et al 2012 ----
fn_AB_01 <- function(B0, B1, B2, B3, N, E, AD){
  
  AB = B0*N^B1*E^B2*AD^B3
  
  return(AB)
  
}

# Ajuste del modelo
fit_AB_01 <- nls(AB ~ fn_AB_01(B0, B1, B2, B3, N, E, AD),
              data = datos |> filter(AD > 0),
              start = c(B0 = 1, B1 = 0.5, B2 = 1, B3 = 0.9))

# Resumen del ajuste
summary(fit_AB_01)

## Tesis Tamarit eq S1 ----
fn_AB_02 <- function(B0, B1, B2, E, AD){
  
  AB = exp(B0)*AD^B1*exp(B2/E)
  
  return(AB)
  
}

# Ajuste del modelo
fit_AB_02 <- nls(AB ~ fn_AB_02(B0, B1, B2, E, AD),
                 data = datos |> filter(AD > 0),
                 start = c(B0 = 0.31, B1 = 1, B2 = 6))

# Resumen del ajuste
summary(fit_AB_02)

## Tipo Schumacher & Hall ----
fn_AB_03 <- function(B0, B1, B2, N, AD){
  
  AB = B0*N^B1*AD^B2
  
  return(AB)
  
}

# Ajuste del modelo
fit_AB_03 <- nls(AB ~ fn_AB_03(B0, B1, B2, N, AD),
              data = datos |> filter(AD > 0),
              start = c(B0 = 1, B1 = 0.5, B2 = 0.9))

# Resumen del ajuste
summary(fit_AB_03)


## Modelo Tesis Tamarit eq = S2 ----
fn_AB_04 <- function(B0, B1, B2, B3, N, E, AD){
  
  AB = exp(B0)*AD^B1*exp((B2+B3*N)/E)
  return(AB)
}

# Ajuste del modelo
fit_AB_04 <- nls(AB ~ fn_AB_04(B0, B1, B2, B3, N, E, AD),
                 data = datos |> filter(AD > 0),
                 start = c(B0 = -0.4, B1 = 1.1, B2 = -36, B3 = 0.02))

# Resumen del ajuste
summary(fit_AB_04)

## Modelo de Pieenar y Shiver (1985) (NO AJUSTA) ----
fn_AB_05 <- function(B0, B1, B2, B3, B4, B5, N, E){
  
  A = B1*(1-exp(-B2*N))
  B = B4*exp(-B5*N)
  AB = B0*(1-exp(-A*(E-B3)))^B
  
  return(AB)
  
}

# Ajuste del modelo
fit_AB_05 <- nls(AB ~ fn_AB_05(B0, B1, B2, B3, B4, B5, N, E),
                 data = datos,
                 start = c(B0 = 56, B1 = 0.08, B2 = 0.008, B3 = 2, B4 = 1.4, B5 = 0.0007))

# Resumen del ajuste
summary(fit_AB_05)


# Modelos de crecimiento --------------------------------------------------

# Chapman-Richards (Ajusta) ----
# Son las mismas estimaciones por nivel de densidad
fn_AB_06 <- function(B0, B1, B2, E){
  AB = B0*(1-exp(-B1*E))^B2
  return(AB)
}

max(datos_notras$AB)  
fit_AB_06 <- nls(AB ~ fn_AB_06(B0 = 56.22, B1, B2, E),
                 data = datos_notras,
                 start = list(B1 = 0.06, B2 = 1.02))  

summary(fit_AB_06)  
  

  
# Chapman-Richards (Ajusta)
fn_AB_07 <- function(B0, B1, B2, E){
  AB = B0*(1-exp(-B1*E))^B2
  return(AB)
}  

fit_AB_07 <- nls(AB ~ fn_AB_07(B0 = 56.22, B1[Densidad], B2, E),
                 data = datos_notras,
                 start = list(B1 = c(0.03, 0.03, 0.03), B2 = 0.8 ))  

summary(fit_AB_07)  
  


# Modelos lineales --------------------------------------------------------

 
## Barrios-Trilleras & López-Aguirre (2020) eq = 1 ----
fn_AB_08 <- function(B0, B1, E){
  AB = (B0+B1*E)/E
  return(AB)
}

fit_AB_08 <- nls(AB ~ fn_AB_08(B0, B1, E),
                 data = datos_notras,
                 start = list(B0 = -3.4, B1 = 2.9))

summary(fit_AB_08)

## Barrios-Trilleras & López-Aguirre (2020) eq = 7 (SI AJUSTA) ----
fn_AB_09 <- function(B0, B1, B2, B3, E, N, AD){
  AB = exp(B0 + B1*(1/E) + B2*log(N) + B3*log(AD))
  return(AB)
}

fit_AB_09 <- nls(AB ~ fn_AB_09(B0, B1, B2, B3, E, N, AD),
                 data = datos_notras,
                 start = list(B0 = -3.4, B1 = -1.6, B2 = 0.41, B3 = 1.1))

summary(fit_AB_09)

## Tesis Roldan (capítulo 2 eq 16 pag 45) (SI AJUSTA, PAR B1 NS) ----
fn_AB_10 <- function(B0, B1, B2, B3, E, AD, N){
  AB = exp(B0)*exp(-B1/E)*AD^B2*N^B3
  return(AB)
}

fit_AB_10 <- nls(AB ~ fn_AB_10(B0, B1, B2, B3, E, AD, N),
                 data = datos_notras,
                 start = list(B0 = 1, B1 = 0.5, B2 = 1, B3 = 0.9))

summary(fit_AB_10)


## Modelo de Schumacher ----
fn_AB_11 <- function(B0, B1, E, N){
  AB = B0*exp(-B1/E)
  return(AB)
}

fit_AB_11 <- nls(AB ~ fn_AB_11(B0, B1, E),
                 data = datos_notras,
                 start = c(B0 = 45, B1 = 1.8))


summary(fit_AB_11)

## Modelo de Uranga 2014b ----
fn_AB_12 <- function(B0, B1, B2, B3, AD, N, E){
  AB = exp(B0)*AD^B1*N^B2*exp(-B3/E)
  return(AB)
}

fit_AB_12 <- nls(AB ~ fn_AB_12(B0, B1, B2, B3, AD, N, E),
                 data = datos_notras,
                 start = c(B0 = 0.69, B1 = 1, B2 = 7.5, B3 = 0.004),
                 control = nls.control(maxiter = 1000, tol = 0.05))


summary(fit_AB_12)


## Barrios-Trilleras & López-Aguirre (2020) eq = 2 ---- 
fn_AB_13 <- function(B0, B1, B2, AD, N){
  AB = exp(B0)+B1*AD*B2*N
  return(AB)
}

fit_AB_13 <- nls(AB ~ fn_AB_13(B0, B1, B2, AD, N),
                 data = datos_notras,
                 # start = list(B0 = -8.1, B1 = 3, B2 = 1),
                 start = list(B0 = 45, B1 = 3, B2 = 1)
                 )

summary(fit_AB_13)










