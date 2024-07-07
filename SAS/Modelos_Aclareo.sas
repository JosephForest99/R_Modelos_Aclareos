/* DATOS DE ALTURA DOMINANTE DE Pinus pseudostrobus Y Abies religiosa */
PROC IMPORT OUT= WORK.DATOS
            DATAFILE= "D:\CP-Doctorado\Examen_Candidatura_2024\R_Modelos_Aclareos\Data\Aclareos.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2;
RUN;


/* MODELOS DE ALTURA DOMINANTE */

/* Chapman-Richards (ANAMÓRFICO) */

PROC MODEL DATA = DATOS;
PARMS B0 = 40 B1 = 0.01 B2 = 1;
AD = B0 * (1 - exp(-B1 * E))**B2;
AD2 = AD * ((1 - exp(-B1 * E2))/(1 - exp(-B1 * E)))**B2;
FIT AD AD2 / SUR;
RUN;

/* Hossfeld IV (POLIMÓRFICO B0) */
PROC MODEL DATA = DATOS;
PARMS B0 = 40 B1 = 0.01 B2 = 1;
AD = E**B2 / (B0 + B1*E**B2);
AD2 = E2**B2 / ((E**B2/AD - B1*E**B2) + B1*E2**B2);
FIT AD AD2 / SUR;
RUN;



/* MODELOS DE AREA BASAL */

/* Tesis Tamarit S1 (SELECCIONADO S4) */
PROC MODEL DATA = DATOS;
PARMS B0 = 0.31 B1 = 1.03 B2 = 6.06;
*B2 = 1;
AB = exp(B0)*AD**B1*exp(B2/E);
AB2 = AB*((AD2**B1*exp(-B2/E2)) / (AD**B1*exp(-B2/E)));
FIT AB AB2 / SUR;
RUN;


/* Tesis Tamarit S2 (SELECCIONADO S1) */
PROC MODEL DATA = DATOS;
PARMS B0 = 0.4 B1 = 1.8 B2 = 36.06 B3 = 0.027;
AB = exp(B0)*AD**B1*exp((B2+B3*N)/E);
AB2 = AB*((AD2**B1*exp((B2+B3*N2)/E2)) / (AD**B1*exp((B2+B3*N)/E)));
FIT AB AB2 / SUR;
RUN;


/* Tesis Tamarit S3 (SELECCIONADO S2) */
PROC MODEL DATA = DATOS;
PARMS B0 = 0.4 B1 = 1.8 B2 = 36.06 B3 = 0.027 B4 = 0.5;
AB = exp(B0)*AD**B1*exp((B2+B3*N+B4*AD)/E);
AB2 = AB*((AD2**B1*exp((B2+B3*N2+B4*AD2)/E2)) / (AD**B1*exp((B2+B3*N+B4*AD)/E)));
FIT AB AB2 / SUR;
RUN;


/* Tesis de Adan Nava eq 2.12 y 2.13 (S3) */
PROC MODEL DATA = DATOS;
PARMS B0 = -2.7 B1 = 1.008 B2 = 0.23 B3 = 0.3;
AB = exp(B0)*AD**B1*E**B2*N**B3;
AB2 = AB * (AD2/AD)**B1 * (E2/E)**B2 * (N2/N)**B3;
FIT AB AB2 / SUR;
RUN;


/* Wences 2015 (S5) */
PROC MODEL DATA = DATOS;
PARMS B0 = -2.7 B1 = 0.028 B2 = -14.23 B3 = 0.7;
AB = exp(B0+B1*E+B2*1/AD)*N**B3;
AB2 = exp( log(AB)+B1*(E2-E)+B2*(1/AD2-1/AD)+B3*log(N2/N) );
FIT AB AB2 / SUR;
RUN;

/* Modelo de Gonzalez-Benecke et al 2012 */

PROC MODEL DATA = DATOS;
PARMS B0 = 1 B1 = 0.5 B2 = 1 B3 = 0.9;
AB = B0*N**B1*E**B2*AD**B3;
AB2 = AB * (N2/N)**B1*(E2/E)**B2*(AD2/AD)**B3;
FIT AB AB2 / SUR;
RUN;


/* MODELOS DE MORTALIDAD */

/* Gonzalez-Benecke et al 2012 (SE PUSO CONSTANTE B2) */
PROC MODEL DATA = DATOS;
PARMS B1 = -0.1 B2 = 0.86;
B2 = 1;
N2 = N * exp(B1*AD*(E2**B2-E**B2));
FIT N2;
RUN;

/*  Santiago-Garcia et al 2015 Eq 9 */
PROC MODEL DATA = DATOS;
PARMS B1 = -0.1;
N2 = N * exp(B1*(E2-E));
FIT N2;
RUN;


/* MODELOS DE VOLUMEN */


/* Tesis Tamarit S1  */
PROC MODEL DATA = DATOS;
PARMS B0 = 0.9 B1 = 1.03 B2 = 0.6 B3 = 0.0002;
V = B0*AB**B1*AD**B2*exp(B3*N/E);
V2 = V * ( (AB**B1*AD2**B2*exp(B3*N2/E2)) / (AB**B1*AD**B2*exp(B3*N/E)) );
FIT V V2 / SUR;
RUN;


/* Tesis Tamarit S2 (SELECCIONADO S1) */
PROC MODEL DATA = DATOS;
PARMS B0 = 2.08 B1 = 1.37 B2 = 9.7 B3 = -1.8 B4 = 2.47;
V = B0*AB**B1*exp(-B2/E)*exp(B3*AB/E+B4*AD/E);
V2 = V * ( (AB2**B1*exp(-B2/E2)*exp(B3*AB2/E2+B4*AD2/E2)) / (AB**B1*exp(-B2/E)*exp(B3*AB/E+B4*AD/E)) ) ;
FIT V V2 / SUR;
RUN;

/* Tesis Tamarit S3 (EL MEJOR) */
PROC MODEL DATA = DATOS;
PARMS B0 = 1.51 B1 = 1.09 B2 = 0.47 B3 = -0.04 B4 = 0.6 B5 = -0.002;
V = B0*AB**B1*AD**B2*N**B3* exp(B4*AD/E+B5*N/E);
V2 = V * ( (B0*AB2**B1*AD2**B2*N2**B3* exp(B4*AD2/E2+B5*N2/E2)) / (AB**B1*AD**B2*N**B3* exp(B4*AD/E+B5*N/E)) ) ;
FIT V V2 / SUR;
RUN;

/* Tesis Adan Nava eq 2.14 (SELECCIONADO S2) */
PROC MODEL DATA = DATOS;
PARMS B0 = 1.51 B1 = 1.09 B2 = 0.47 B3 = -0.04;
V = exp(B0)*AB**B1*AD**B2*N**B3;
V2 = V * (AB2/AB)**B1 * (AD2/AD)**B2 * (N2/N)**B3 ;
FIT V V2 / SUR;
RUN;



/* MODELOS PARA ESTIMAR EL CI */

DATA DATOS_CI;
SET DATOS;
WHERE ACLAREO = 'CA';
RUN;


/* Modelo de Gonzalez-Beckere 2015 */
PROC MODEL DATA = DATOS;
PARMS B1 = -1.5;
CI2 = CI * exp((B1/E2) * (E2-E));
FIT CI2;
RUN;
