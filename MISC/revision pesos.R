library(readr)
library(tidyverse)

master_pesos <- read_csv("generar ponderadores/resultados/master_pesos.csv")
pesos_longitudinales_elsoc <- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")


load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")
#load("C:/RProjects/ELSOC 2022/modelamiento_participa/DATOS/DATAVERSE/elsoc_long.RData")




elsoc_long_2016_2022=elsoc_long_2016_2022%>%
  sjmisc::set_na(na=c(-666,-777,-888,-999))%>%
  mutate(phq=Reduce('+',across(starts_with("s11_0"),
                               ~ car::recode(.x, "1=0;2=1;3=2;c(4,5)=3"))),
         s10=as.numeric(phq>9),
         satis_demo=as.numeric(c01 %in% c(4,5)),
         educ= factor(car::recode(m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                      labels = c("Básica","Media","Técnica","Universitaria")),
         tramo_etario= factor(case_when(
           m0_edad< 30~1,
           m0_edad >29&m0_edad <40~2,
           m0_edad>39& m0_edad < 50~3,
           m0_edad>49 &m0_edad <60~4,
           m0_edad >59 ~5),
           labels = c("18 a 29","30 a 39","40 a 49","50 a 59","60 o más")))

pesos_elsoc<- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")

elsoc_pesos =elsoc_long_2016_2022%>%
  left_join(pesos_elsoc,by=c("idencuesta","ola"))


elsoc_merge=elsoc_long_2016_2022%>%
  left_join(pesos_longitudinales_elsoc,by=c("idencuesta","ola"))








grafo_box_var("muestra")
grafo_box_var("m0_sexo")
grafo_box_var("educ")
grafo_box_var("tramo_etario")


grafo_pesos("muestra")
grafo_pesos("m0_sexo")
grafo_pesos("educ")
grafo_pesos("tramo_etario")






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



