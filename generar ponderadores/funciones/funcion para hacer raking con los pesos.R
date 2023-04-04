library(tidyverse)  
library(survey)


#DEFINIR TOTALES

totales_ine_edad <- read_csv("datos/insumos_ponderadores/ine/totales_ine_edad.csv")
totales_ine_sexo <- read_csv("datos/insumos_ponderadores/ine/totales_ine_sexo.csv")


# CAGAR PESOS ORIGINALES


rake_pesos<- function(base,pesos,años){
  
  pesos_por_año <-lapply(años,
                         function(i){
                           data = base%>%filter(ola ==i)
                           
                           svd_diseno <- svydesign(id=~segmento_disenno, weights =formula(paste0("~",pesos)),data=data) 
                           
                           
                           pop_sexo <- data.frame(m0_sexo=filter(totales_ine_sexo,año==i)$sexo, 
                                                  Freq=filter(totales_ine_sexo,año==i)$total)
                           
                           pop_edad <- data.frame(tramo_etario=filter(totales_ine_edad,año==i)$tramo_etario, 
                                                  Freq=filter(totales_ine_edad,año==i)$total)
                        # Revisar funcion rake() del paquete survey (Tambien revisar capitulo 7 del lumley 2012)   
                           svd_diseno_rk <-rake(svd_diseno, list(~m0_sexo,~tramo_etario), list(pop_sexo,pop_edad))
                           
                           
                           data$pd_rk <- weights(svd_diseno_rk)
                           
                           data_out <- data%>%
                             select(idencuesta,ola, pd_rk)
                           return(data_out)                  
                         })%>%
    bind_rows()
  

  return(pesos_por_año)    
}

