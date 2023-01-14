#VARIABLES DE DURACION
library(tidyverse)
library(readxl)

load("datos/oficiales/ELSOC_Wide_2016_2022_v1.00_R.RData")



elsoc_long <- select(elsoc_wide_2016_2022,idencuesta,muestra, 
                    contains("dur"),
                    starts_with("n_visitas"),
                    starts_with("ponderador02"))%>%
  pivot_longer(cols=contains('_w'),
               names_to = c('.value','.ola'),
               names_sep = '_w')%>%
  rename(ola=.ola)%>%
  select(idencuesta,ola,muestra,everything())%>%
  mutate(responde=as.numeric(!is.na(ponderador02)),
         ola=as.numeric(ola))%>%
  filter(!(muestra==2&ola==1),!(muestra==2&ola==2))


  
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



elsoc_dur_entr = elsoc_long %>% 
  mutate(duracion=ifelse(responde==1,
                         apply(select(.,contains("mod")),1,sum,na.rm=TRUE),NA))%>%
  arrange(idencuesta,ola)%>%
  group_by(idencuesta)%>%
  mutate(visitas_locf = imp_locf(n_visitas_entr),
         visitas_1ra= ifelse(muestra==1,rep(visitas_locf[1],6),rep(visitas_locf[1],4)),
         visitas_lag=lag(visitas_locf),
         duracion_locf=imp_locf(duracion),
         duracion_lag=lag(duracion_locf),
         duracion_1ra=ifelse(muestra==1,rep(duracion_locf[1],6),rep(duracion_locf[1],4)))%>%
  select(idencuesta,ola,muestra,responde,n_visitas_entr,
         visitas_locf,visitas_1ra,visitas_lag,duracion,duracion_locf,duracion_lag,duracion_1ra)%>%
  ungroup()


# NO RESPUESTA AL ITEM ----------------------------------------------------

load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")
libro_codigos <- read_excel("datos/libro_codigos.xlsx")

notin_long <- setdiff(libro_codigos$codigo_longitudinal,
                      names(elsoc_long_2016_2022))


vars <- libro_codigos%>%
  filter(!(modulo%in% "Otras Variables"), 
         !codigo_longitudinal%in%notin_long)



elsoc_modulos <- elsoc_long_2016_2022%>%
  select(!!vars$codigo_longitudinal)




nas <- lapply(names(elsoc_modulos),
              FUN = function(i){
                tabla <-tibble("var"=getElement(elsoc_modulos,i) %in% c(-888,-999))
                names(tabla)<- i
                return(tabla)})%>%bind_cols()


# POR MODULO
modulos <- unique(vars$modulo)

n_modulos <- vars%>% group_by(modulo)%>%count()

nas_modulos <-lapply(1:length(modulos), function(j){
  
  var_modulo <- vars %>% 
    filter(modulo ==modulos[j])  
  
  tabla <-tibble(variable=apply(select(nas, !!var_modulo$codigo_longitudinal),1,sum))
  
  names(tabla)<- janitor::make_clean_names(modulos[j])
  
  return(tabla)})%>%bind_cols()

# PROP POR MODULO

meds_modulo <- lapply(1:length(modulos), function(j){
  
  var_modulo <- vars %>% 
    filter(modulo ==n_modulos$modulo[j])  
  
  tabla <-tibble(variable=apply(select(nas, !!var_modulo$codigo_longitudinal),1,sum))%>%
    mutate(meds=variable > median(variable))%>%
    select(meds)
  
  names(tabla)<- janitor::make_clean_names(n_modulos$modulo[j])
  
  return(tabla)
  
})%>%bind_cols()


elsoc_itemnrns <- elsoc_long_2016_2022%>%
  mutate(nsnr_total=apply(nas,1,sum),
         nsnr_median_mod=apply(meds_modulo,1,sum),
         nsnr_prom_mod=apply(meds_modulo,1,mean))%>%
  select(idencuesta,ola,muestra, nsnr_total,nsnr_median_mod,nsnr_prom_mod)

# COMBINAR BASES ----------------------------------------------------------


  
elsoc_prd =elsoc_dur_entr%>%
  left_join(elsoc_itemnrns,by=c("idencuesta","ola","muestra"))%>%
  group_by(idencuesta)%>%
  mutate(nsnr_total_locf=imp_locf(nsnr_total),
         nsnr_total_1ra=ifelse(muestra==1,rep(nsnr_total_locf[1],6),rep(nsnr_total_locf[1],4)),
         nsnr_total_lag=lag(nsnr_total_locf),
         nsnr_median_mod_locf=imp_locf(nsnr_median_mod),
         nsnr_median_mod_1ra=ifelse(muestra==1, rep(nsnr_median_mod_locf[1],6),rep(nsnr_median_mod_locf[1],4)),
         nsnr_median_mod_lag=lag(nsnr_median_mod_locf),
         nsnr_prom_mod_locf=imp_locf(nsnr_prom_mod),
         nsnr_prom_mod_1ra=ifelse(muestra==1,rep(nsnr_prom_mod[1],6),rep(nsnr_prom_mod[1],4)),
         nsnr_prom_mod_lag=lag(nsnr_prom_mod_locf))%>%
  ungroup()

    

saveRDS(elsoc_prd,file="datos/insumos_ponderadores/elsoc_prd.RDS")