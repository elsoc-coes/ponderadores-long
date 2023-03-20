library(tidyverse)
library(Hmisc)
pesos_elsoc <- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")
load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")

elsoc_long_2016_2022=elsoc_long_2016_2022%>%
  sjmisc::set_na(na=c(-666,-777,-888,-999))%>%
  mutate(phq=Reduce('+',across(starts_with("s11_0"),
                               ~ car::recode(.x, "1=0;2=1;3=2;c(4,5)=3"))),
         phq_s10=as.numeric(phq>9),
         satis_demo=as.numeric(c01 %in% c(4,5)),
         educ= factor(car::recode(m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                      labels = c("Básica","Media","Técnica","Universitaria")),
         tramo_etario= factor(case_when(
           m0_edad< 30~1,
           m0_edad >29&m0_edad <40~2,
           m0_edad>39& m0_edad < 50~3,
           m0_edad>49 &m0_edad <60~4,
           m0_edad >59 ~5),
           labels = c("18 a 29","30 a 39","40 a 49","50 a 59","60 o más")),
         interes_poli=as.numeric(c13 %in% c(4,5)),
         confianza=as.numeric(c02 ==1),
         autori=as.numeric(c25==2),
         estatus_sub=as.numeric(d01_01 %in% c(4:6)),
         satis_vida=as.numeric(s01 %in% c(4,5)),
         seguridad=as.numeric(t10 %in% c(4,5)),
         contacto=as.numeric(r06 %in% c(4,5)),
         muestra=factor(muestra,labels=c("Original","Refresco")),
         m0_sexo=factor(m0_sexo,labels=c("Hombre","Mujer")),
         estrato_disenno=factor(estrato_disenno,
                                labels = c( names(attr(getElement(.,"estrato_disenno"),"labels")))))%>%
  left_join(pesos_elsoc,by=c("idencuesta","ola"))


elsoc_long_2016_2022$ponderadorlong_trim_6 <-ifelse(getElement(elsoc_long_2016_2022,"ponderadorlong_total")>=6,
                                                            6,
                                                          getElement(elsoc_long_2016_2022,"ponderadorlong_total"))  



elsoc_long_2016_2022$ponderadorlong_trim_4 <-ifelse(getElement(elsoc_long_2016_2022,"ponderadorlong_total")>=4,
                                                    4,
                                                    getElement(elsoc_long_2016_2022,"ponderadorlong_total"))  

elsoc_long_2016_2022%>%
  group_by(ola)%>%
  reframe(wtd.var(phq,ponderador01))

elsoc_long_2016_2022%>%
  group_by(ola)%>%
  reframe(wtd.var(phq,ponderadorlong_total))





elsoc_long_2016_2022%>%
  group_by(ola)%>%
  reframe(across(c(ponderador02,ponderadorlong_total,
                   ponderadorlong_trim_6,ponderadorlong_trim_4),
                 ~wtd.var(phq,.x)))%>%
  group_by(ola)%>%
  reframe(across(c(ponderadorlong_total,ponderadorlong_trim_6,ponderadorlong_trim_4),
                 ~.x/ponderador02))





elsoc_long_2016_2022%>%
  group_by(ola)%>%
  reframe(across(c(ponderador02,ponderadorlong_total,
                   ponderadorlong_trim_6,ponderadorlong_trim_4),
                 ~wtd.var(phq,.x)))%>%
  group_by(ola)%>%
  reframe(across(c(ponderadorlong_total,ponderadorlong_trim_6,ponderadorlong_trim_4),
                 ~.x/ponderador02))%>%
  reframe(across(-ola,~mean(.x)))%>%
  View()

eficiencia_var <- function(var){

  elsoc_long_2016_2022%>%
    group_by(ola)%>%
    reframe(across(c(ponderador01,ponderador02,ponderadorlong_panel,ponderadorlong_trim_6,ponderadorlong_trim_4),
                   ~wtd.var(!!rlang::sym(var),.x)))
  }


eficiencia_var("interes_poli")
