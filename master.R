source("datos/codigo/0-crear bases long_wide.R")

source("datos/codigo/I-A-crear base imputada a partir de base long wide.R")
source("datos/codigo/I-B-Crear base proceso recoleccion datos.R")
source("datos/codigo/I-C-crear bases CIT.R")


# MODELAMIENTO NR ---------------------------------------------------------


source("datos/codigo/II-crear base modelo.R")


# IMPUTAR VALORES EN BASE MODELO
source("modelamiento NR/predicciones/III-A-crear base modelo con valores imputados.R")
#HACER PREDICCION
source("modelamiento NR/predicciones/III-B-generar predicciones nr base imputada.R")


# GENERAR PESOS ----------------------------------------------------------------

source("generar ponderadores/IV-generar master pesos.R")


