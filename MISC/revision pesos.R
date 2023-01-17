library(readr)
library(tidyverse)

master_pesos <- read_csv("generar ponderadores/resultados/master_pesos.csv")
pesos_longitudinales_elsoc <- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")


load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")




master_pesos%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
    across(,~sum(is.na(.x))),
    panel=sum(tipo_atricion==1,na.rm=TRUE))



pesos_longitudinales_elsoc%>%
  group_by(ola)%>%
  summarise(n=n(),
            across(,~sum(is.na(.x))))


elsoc_merge=elsoc_long_2016_2022%>%
            left_join(pesos_longitudinales_elsoc,by=c("idencuesta","ola"))


elsoc_merge%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
    across(starts_with("ponderador"),~sum(is.na(.x))),
    panel=sum(tipo_atricion==1,na.rm = TRUE))



elsoc_merge%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
            prop_mujer=mean(m0_sexo ==2,na.rm=TRUE),
            w_prop_mujer=weighted.mean(m0_sexo ==2,w=ponderadorlong_total,na.rm=TRUE))%>%
  View()



elsoc_merge%>%
  group_by(ola)%>%
  summarise(n=n(),
            prop_mujer=mean(m0_sexo ==2,na.rm=TRUE),
            w_prop_mujer=weighted.mean(m0_sexo ==2,w=ponderadorlong_total,na.rm=TRUE))%>%
  View()
