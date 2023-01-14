library(tidyverse)

elsoc_long <- readRDS("datos/insumos_ponderadores/elsoc_longwide.RDS")


impu_edad<- function(valores){
  posi<- which(!is.na(valores))
  unicos<- unique(na.omit(valores))
  unicos[1]-posi[1]+1
}

imp_locf<- function(valores){
  if(length(unique(na.omit(valores)))==0){
    valores
  }else{
    if(is.na(valores[1])){
      valores[1] <- unique(na.omit(valores))[1]
    }
    zoo::na.locf(valores)  
  }
}


elsoc_imp <- elsoc_long%>%
  group_by(idencuesta)%>%
  mutate(sexo_imp= imp_locf(m0_sexo),
         edu_imp=imp_locf(m01_fac),
         edad_imp=impu_edad(m0_edad),
         civil_imp=imp_locf(m36_fac))%>%
  ungroup()%>%
  mutate(sexo_imp=factor(sexo_imp,labels = c('Hombre','Mujer')),
         edad_imp_tramo=factor(car::recode(edad_imp,"18:29=1;30:44=2;45:59=3;60:88=4"),
                               labels =  c("18 a 29","30 a 44","45 a 59","60 0 más")),
         ola=factor(ola),
         muestra=factor(muestra,labels = c('Original','Refresco')),
         responde=factor(responde,labels=c("No Responde","Responde")))%>%
  select(idencuesta,ola,muestra,responde,
         estrato_disenno,segmento_disenno,sexo_imp,edad_imp_tramo,
         edu_imp,civil_imp, ola_num)

data.table::fwrite(elsoc_imp,file = "datos/insumos_ponderadores/elsoc_imp.csv")
