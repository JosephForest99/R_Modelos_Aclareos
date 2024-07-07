

# SCR de Pieenar & Schiver (1985) -----------------------------------------

# Modelo de mortalidad de Pieenar y Schiver (1985)
fn_S_PS1985 <- function(N, E){
  B0 = 0.004183
  B1 = 0.0006591
  B2 = 58.146
  B3 = 0.003558
  B4 = 1.3837
  
  lambda = B0*exp(B1*N)
  theta = B2*exp(-B3*N)
  S = exp(-lambda*(E-theta)^B4)
  return(S)
}

fn_S_PS1985(800, 10.3)


# Modelo de AB

fn_AB_05(B0 = 242, B1=0.085450, B2=0.0084585, B3=2.0362, B4=.4261, B5=0.0007143,
         N = 920, E=10.3)