library(tidyverse)
library(data.table)


load("datos/oficiales/ELSOC_Wide_2016_2022_v1.00_R.RData")

elsoc_long <-select(elsoc_wide_2016_2022,idencuesta,muestra, 
                    starts_with("ponderador02"),
                    starts_with("m0_sexo"),starts_with("m02"),
                    starts_with("m01"),starts_with("m36"),-starts_with("m36_otro"),
                    starts_with("m0_edad"),starts_with("estrato_disenno"),
                    starts_with("segmento_disenno"),starts_with("n_visitas_entr"),
                    ends_with("dur"))%>%
  pivot_longer(cols=contains('_w'),
               names_to = c('.value','.ola'),
               names_sep = '_w')%>%
  rename(ola=.ola)%>%
  mutate(ola_num=car::recode(ola,"'01'='2016';'02'='2017';'03'='2018';'04'='2019';'05'='2021';'06'='2022'")-2016,
         ola=factor(car::recode(ola,"'01'='2016';'02'='2017';'03'='2018';'04'='2019';'05'='2021';'06'='2022'")))%>%
  select(idencuesta,ola,ola_num,muestra,everything())


# DATOS PERDIDOS ----------------------------------------------------------
elsoc_long <-sjmisc::set_na(elsoc_long,na=c(-666,-777,-888,-999))


# VARIABLE RESPUESTA ------------------------------------------------------


elsoc_long$responde <- as.numeric(!is.na(elsoc_long$ponderador02))

# Segmento a factor
elsoc_long$segmento_disenno<- factor(elsoc_long$segmento_disenno)
elsoc_long$estrato_disenno<- factor(elsoc_long$estrato_disenno,
                                    labels = names(attr(elsoc_long$estrato_disenno,"labels")))

elsoc_long<- elsoc_long%>%
  filter(!(ola%in%c("2016","2017")&muestra==2))


# SOCIODEMOGRAFICOS -------------------------------------------------------

#Actividad principal

elsoc_long$m02_fac<-factor(car::recode(elsoc_long$m02, "c(1,2,3)=1;7=2;5=3;6=4;c(4,8,9)=5"),
                           labels = c("Trabajo Remunerado","Trabajo Doméstico No Remunerado","Jubilado","Desempleado","Otras Categorías"))

setattr(elsoc_long$m02_fac,"label","Actividad principal entrevistado")
#Nivel educacional
elsoc_long$m01_fac<- factor(car::recode(elsoc_long$m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                            labels = c("Básica","Media","Técnica","Universitaria"))
setattr(elsoc_long$m01_fac, "label","Nivel educacional")
#Estado civil 
elsoc_long$m36_fac <- factor(car::recode(elsoc_long$m36,"c(1,2,3)=1;c(4,5,6,7,8)=2;else=NA"),
                             labels = c("En Pareja","Soltero"))
setattr(elsoc_long$m36_fac,"label","Estado Civil")

saveRDS(elsoc_long,"datos/insumos_ponderadores/elsoc_longwide.RDS")

data.table::fwrite(elsoc_long,"datos/insumos_ponderadores/elsoc_longwide.csv")
