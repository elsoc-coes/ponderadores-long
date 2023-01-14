  library(tidyverse)
  library(gtools)
  library(gee)
  library(geepack)

m1 <- readRDS("datos/insumos_ponderadores/m1.RDS")%>%
      mutate(ola_fac=factor(ola_fac))%>%
      drop_na()


m2 <- readRDS("datos/insumos_ponderadores/m2.RDS")%>%
  select(-zona,-ciudad,-comuna)%>%
  mutate(ola_fac=factor(ola_fac))%>%
  drop_na()

covariables <- c("ola_fac","estrato_disenno","sexo_imp","edad_imp_tramo","edu_imp","civil_imp",
                "nsnr_prom_mod_1ra_c","visitas_lag_c","densidad_pob_km2_c","jh_esc_m_c",
                "prop_hacina_c","tamagno_hog_c","cant_pers_prom_c","prop_precaria_c")




back_gee <-function(base,vars,metrica){
  
  name_covs <- vars
  
  while(TRUE){
    modelo_all <- geeglm(reformulate(response = "responde",name_covs),
                         family = binomial,
                         id = idencuesta,
                         corstr = 'ar1',
                         data=na.omit(base))
    
    combis <- combinations(length(name_covs),length(name_covs)-1,name_covs)
    
    ecuaciones <- lapply(1:nrow(combis),function(i){reformulate(response="responde",combis[i,])})
    
    
    modelos <- lapply(1:length(ecuaciones),function(j){geeglm(ecuaciones[[j]],
                                                              family = binomial,
                                                              id = idencuesta,
                                                              corstr = 'ar1',
                                                              data=na.omit(base))})  
    
    var_all<- labels(terms(modelo_all$formula))
    
    
    tabla<- tibble("out"=sapply(1:length(modelos),
                                function(i){setdiff(var_all, 
                                            labels(terms(modelos[[i]]$formula)))}))%>%
            mutate(valor=sapply(modelos, function(i){QIC(i)[metrica]}),
                   dife=QIC(modelo_all)[metrica]-valor)
    
    
    #SALIDA LOOP
    if(QIC(modelo_all)[metrica]-min(tabla$valor)<1) {break} 
    else{
      name_covs <-labels(terms(modelos[[which.min(tabla$valor)]]))
    print(list("fuera"=tabla[which.min(tabla$valor),]$out,
               "gain"=tabla[which.min(tabla$valor),]$dife))}  
  }
  
  print(paste("Modelo final: ",modelo_all$formula)[3])
  return(modelo_all)
  
}




bestGEE_QICU<- back_gee(m1,covariables,"QICu")

save(bestGEE_QICU,file = "modelamiento NR/modelos/bestGEE_QICU.RDS",
     compression_level = 9)


bestGEE_QICU_m2 <- back_gee(m2,covariables,"QICu")

save(bestGEE_QICU_m2,
        file = "modelamiento NR/modelos/bestGEE_QICU_m2.RDS",
        compression_level = 9)

