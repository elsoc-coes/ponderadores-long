library(readr)
library(tidyverse)
library(data.table)

master_pesos <- read_csv("generar ponderadores/resultados/master_pesos.csv")%>%
                select(-c(tipo_atricion,segmento_disenno,estrato_disenno,m0_sexo))
pesos_longitudinales_elsoc <- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")


load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")

elsoc_long_2016_2022= sjmisc::set_na(elsoc_long_2016_2022,na=c(-666,-777,-888,-999))

elsoc_merge = elsoc_long_2016_2022%>%
              left_join(master_pesos)%>%
              mutate(phq=Reduce('+',across(starts_with("s11_0"),
                                           ~ car::recode(.x, "1=0;2=1;3=2;c(4,5)=3"))),
                     s10=as.numeric(phq>9),
                     satis_demo=as.numeric(c01 %in% c(4,5)),
                     educ= factor(car::recode(m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                                  labels = c("Básica","Media","Técnica","Universitaria")))%>%
              select(idencuesta,ola,s10,satis_demo,muestra,m0_sexo,tramo_etario,educ,
                     starts_with("ponderador"))

fwrite(elsoc_merge,file="MISC/bases/elsoc_merge.csv")