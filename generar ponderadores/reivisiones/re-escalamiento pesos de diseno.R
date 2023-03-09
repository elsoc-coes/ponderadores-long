factores_expansion <- readRDS("datos/insumos_ponderadores/factores_expansion.RDS")%>%
  select(idencuesta,ola,pd_diseno,pd_nr,muestra)


# RE-ESCALAR
source("generar ponderadores/funciones/funcion para re escalar los pesos.R")

factores_expansion$diseno_rs=rs_pesos(factores_expansion,"pd_diseno")

factores_expansion%>%
  group_by(ola)%>%
  summarise(n=n(), suma=sum(diseno_rs),minimo=min(diseno_rs),maximo=max(diseno_rs))

