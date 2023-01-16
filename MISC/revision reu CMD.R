library(readr)
library(tidyverse)

load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")
factores_expansion <- readRDS("datos/insumos_ponderadores/factores_expansion.RDS")%>%
                      mutate(pd_elsoc=ifelse(ola==3,pd_muestra,pd_elsoc))




elsoc_long_2016_2022%>%
  group_by(ola)%>%
  summarise(n=n(),
    p01=sum(is.na(ponderador01)),
            p02=sum(is.na(ponderador02)))


elsoc_long_2016_2022%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
    across(starts_with("ponderador"),sum,na.rm=TRUE))%>%
  arrange(ola)



factores_expansion%>%
  group_by(muestra, ola)%>%
  summarise(n=n(),
            across(starts_with("pd"), sum))%>%
  arrange(ola)




# VALORES UNICOS DISENO ---------------------------------------------------

factores_expansion%>%
  group_by(idencuesta)%>%
  summarise(across(starts_with("pd"),~length(unique(.x))))%>%
  count(pd_diseno)


#Diferencias
dif_factores=factores_expansion%>%
  group_by(idencuesta)%>%
  mutate(across(starts_with("pd"),~.x-lag(.x),.names = "dif_{col}"))%>%
  arrange(idencuesta)%>%
  select(idencuesta,ola,muestra,starts_with("dif"))


#Promedio diferencias por año

dif_factores%>%
     group_by(muestra,ola)%>%
     summarise(across(starts_with("dif"),mean,na.rm=TRUE))


dif_factores%>%
  group_by(muestra,ola)%>%
  summarise(across(starts_with("dif"),mean,na.rm=TRUE))



#Años donde se mantiene
dif_factores%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
            across(starts_with("dif"),~sum(.x==0,na.rm=TRUE)))

#Diferencias diseño y no respues

factores_expansion%>%
  group_by(muestra,ola)%>%
  summarise(n=n(),
            igual=sum((pd_diseno-pd_nr)==0,na.rm = TRUE))%>%
  mutate(prop=igual/n)
