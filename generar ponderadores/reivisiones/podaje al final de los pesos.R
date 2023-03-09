source("generar ponderadores/reivisiones/prueba para generar pesos con distintos trim.R")


load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")
pesos_longitudinales_elsoc <- read_csv("generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")


afectados<-elsoc_long_2016_2022%>%
  left_join(pesos_longitudinales_elsoc,by=c("ola","idencuesta"))%>%
  group_by(muestra,ola)%>%
  reframe(n=sum(ponderadorlong_total>=6))


elsoc_long_2016_2022%>%
  left_join(pesos_longitudinales_elsoc,by=c("ola","idencuesta"))%>%
  group_by(muestra,ola)%>%
  reframe(enframe(quantile(ponderadorlong_total, probs=seq(0,1,.1))))%>%
  mutate(name=factor(name,levels=c("0%","10%","20%", "30%","40%","50%", "60%","70%","80%","90%","100%")))%>%
  spread(name,value)

mean(afectados$n)
peso_1_91<- generar_pesos(1,91)

peso_2_92<- generar_pesos(2,92)

peso_3_93<- generar_pesos(3,93)

peso_4_94<- generar_pesos(4,94)

peso_5_95<- generar_pesos(5,95)


lapply(list(peso_1_91,peso_2_92,peso_3_93,peso_4_94,peso_5_95))


min(peso_1_91$ponderadorlong_total)
min(peso_2_92$ponderadorlong_total)

min(peso_4_94$ponderadorlong_total)


min(peso_5_95$ponderadorlong_total)


max(peso_2_92$ponderadorlong_total)

max(peso_4_94$ponderadorlong_total)


max(peso_5_95$ponderadorlong_total)


trim_pesos<- function(valor_maximo=NULL){
  
  
if(!is.null(valor_maximo)){
  
pesos_longitudinales_elsoc$ponderadorlong_total<- ifelse(getElement(pesos_longitudinales_elsoc,"ponderadorlong_total")>=valor_maximo,
         valor_maximo,
         getElement(pesos_longitudinales_elsoc,"ponderadorlong_total"))  
  
}
  
  tabla = elsoc_long_2016_2022%>%
    left_join(pesos_longitudinales_elsoc,by=c("ola","idencuesta"))%>%
    group_by(muestra,ola)%>%
    reframe(muestral=mean(m0_sexo==2),
            pond=weighted.mean(m0_sexo==2,
                               w=ponderadorlong_total))
  return(tabla)
}


trim_pesos()

