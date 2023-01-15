library(tidyverse)
library(readr)


ine_2002_2035 <- read_csv("datos/insumos_ponderadores/ine/proy_ine/ine_2002-2035.csv")



ine_pob_objetivo<- ine_2002_2035%>%
  janitor::clean_names()%>%
  gather(año,poblacion, contains("20"))%>%
  mutate(año=parse_number(año),
         sexo=ifelse(sexo==1,"Hombre","Mujer"))%>%
  filter(edad >17,area==1)

totales_ine_sexo <- ine_pob_objetivo%>%
  group_by(año,sexo)%>%
  summarise(total=sum(poblacion))%>%
 filter(año %in% 2016:2022)


write.csv(totales_ine_sexo,file="datos/insumos_ponderadores/ine/totales_ine_sexo.csv",row.names = FALSE )

totales_ine_edad <- ine_pob_objetivo%>%
  group_by(año,edad)%>%
  summarise(total=sum(poblacion))%>%
  filter(año %in% 2016:2022)%>%
  mutate(tramo_etario= factor(case_when(
    edad< 30~1,
    edad >29&edad <40~2,
    edad>39& edad < 50~3,
    edad>49 & edad <60~4,
    edad >59 ~5),
  labels = c("18 a 29","30 a 39","40 a 49","50 a 59","60 o más")))%>%
  group_by(año,tramo_etario)%>%
  summarise(total=sum(total))
  
write.csv(totales_ine_edad,file="datos/insumos_ponderadores/ine/totales_ine_edad.csv",row.names = FALSE )
  
  
  