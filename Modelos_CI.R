
# Modelos de CI -----------------------------------------------------------

## Modelo de Gonzalez-Benecke et al 2012

fn_CI_00 <- function(B1, CI1, E1, E2){
  
  CI2 = CI1 * exp((B1/E2) * (E2-E1))
  return(CI2)
}

fit_CI_00 <- nls(CI2 ~ fn_CI_00(B1, CI, E, E2),
                 data = datos_notras ,
                 start = c(B1 = -2.5))

# Resumen del ajuste
summary(fit_CI_00)


## Modelo de Barrios-Trilleras (NO AJUSTA) ----
fn_CI_01 <- function(B1, B2, CI1, E1, E2, AD1, AD2){
  
  CI2 = CI1 * exp(B1*(1-E1/E2) + B2*(log(AD2-AD1)))
  return(CI2)
}

library(minpack.lm)
fit_CI_01 <- nls(CI2 ~ fn_CI_01(B1, B2, CI, E, E2, AD, AD2),
                 data = datos_notras |> filter(Aclareo == "CA"),
                 start = c(B1 = -0.6, B2 = -0.7))

# Resumen delnlsLM()# Resumen del ajuste
summary(fit_CI_01)


## Modelo de Pieenar y Schiver (1984)
fn_CI_02 <- function(B1, B2, B3, CI1, E1, E2){
  CI2 = CI1*exp(-(B1+B2*CI1)*((E2-E1)/E1)^B3)
  return(CI2)
}

fit_CI_02 <- nls(CI2 ~ fn_CI_02(B1, B2, B3, CI, E, E2),
                 data = datos_notras |> filter(Aclareo == "CA"),
                 start = c(B1 = 1.04, B2 = -0.60, B3 = 0.808))

summary(fit_CI_02)


## Modelo exponencial
fn_CI_03 <- nls(CI2~ CI1*exp(AD^B1*))
