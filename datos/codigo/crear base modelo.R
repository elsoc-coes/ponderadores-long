library(tidyverse)
elsoc_imp <- readr::read_csv("datos/insumos_ponderadores/elsoc_imp.csv")
CIT <- readr::read_csv("DATOS/CIT/BASES ORI/bases_CIT.csv")
elsoc_prd <- readRDS("DATOS/PRD/elsoc_prd.RDS")


elsoc_imp <- readr::read_csv("DATOS/IMPUTACION/elsoc_imp.csv")%>%
  mutate(muestra=ifelse(muestra=="Original",1,0),
         ola=car::recode(ola,'2016=1;2017=2;2018=3;2019=4;2021=5'))


CIT <- readr::read_csv("DATOS/CIT/BASES ORI/bases_CIT.csv")%>%
  mutate(muestra=ifelse(muestra=="Original",1,0),
         ola=car::recode(ola,'2016=1;2017=2;2018=3;2019=4;2021=5'))



elsoc_prd <- readRDS("DATOS/PRD/elsoc_prd.RDS")%>%
  mutate(muestra=ifelse(muestra==1,1,0))


base_modelo <-list(elsoc_imp, 
     select(CIT,idencuesta,ola,muestra,zona,ciudad,comuna,densidad_pob_km2,
            jh_esc_m,prop_hacina, tamagno_hog,
            cant_pers_prom,prop_precaria),
     select(elsoc_prd,idencuesta,ola,muestra,idencuesta,ola,muestra,
            visitas_1ra,visitas_lag,nsnr_total_1ra,
            nsnr_total_lag,nsnr_median_mod_1ra,nsnr_median_mod_lag,nsnr_prom_mod_1ra,nsnr_prom_mod_lag))%>%
  purrr::reduce(left_join,by=c("idencuesta","ola","muestra"))%>%
  select(idencuesta,ola,muestra,estrato_disenno, segmento_disenno,zona,ciudad,comuna,responde,everything())



data.table::fwrite(base_modelo,file = "DATOS/base_modelo_sin_centrar.csv")

# CENTRAR LAS VARIABLES ---------------------------------------------------


centrar<-function(x){
  
  tabla<-tibble(valor=  getElement(base_modelo,x)-mean(getElement(base_modelo,x),na.rm=TRUE))
  names(tabla)<- paste(x,"_c",sep="")
  return(tabla)
}

vars_a_centrar<- c("nsnr_total_1ra","nsnr_total_lag","nsnr_median_mod_1ra",
                   "visitas_1ra","visitas_lag", "nsnr_prom_mod_1ra","nsnr_prom_mod_lag",
                   "densidad_pob_km2","jh_esc_m","prop_hacina","tamagno_hog","cant_pers_prom",
                   "prop_precaria","nsnr_median_mod_lag")

base_modelo <- select(base_modelo,-!!vars_a_centrar)%>%
  bind_cols(bind_cols(lapply(vars_a_centrar,centrar)))%>%
  mutate(ola_fac=factor(ola,labels = c("2016","2017","2018","2019","2021")),
         responde=as.numeric(responde=="Responde"))


m1 <- base_modelo%>%
  filter(ola!=1&muestra==1)%>%
  mutate(ola_num=ola_num-1)

m2 <- base_modelo%>%
  filter(ola!=3&muestra==0)%>%
  mutate(ola_num=ola_num-1)


saveRDS(m1,file="DATOS/m1.RDS")
saveRDS(m2,file="DATOS/m2.RDS")

data.table::fwrite(base_modelo,file = "DATOS/base_modelo.csv")
