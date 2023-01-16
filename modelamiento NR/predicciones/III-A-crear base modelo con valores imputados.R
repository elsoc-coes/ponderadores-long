library(gee)
library(geepack)

base_modelo <- read_csv("datos/insumos_ponderadores/base_modelo.csv")


bm_territorial =
  select(base_modelo,idencuesta,ola_fac,edu_imp,sexo_imp,edad_imp_tramo,estrato_disenno,densidad_pob_km2_c,jh_esc_m_c,
                 cant_pers_prom_c,prop_precaria_c,nsnr_prom_mod_1ra_c,visitas_1ra_c,prop_hacina_c,tamagno_hog_c)


variables_imputar=c( "densidad_pob_km2_c","jh_esc_m_c",
                     "prop_precaria_c","prop_hacina_c",
                     "tamagno_hog_c","cant_pers_prom_c",
                     "nsnr_prom_mod_1ra_c")

lista_modelos = lapply(variables_imputar,function(i){
  
  
  if(i=="nsnr_prom_mod_1ra_c"){
    modelo=geeglm(reformulate(response = i,termlabels = c("edu_imp","sexo_imp","edad_imp_tramo","estrato_disenno")),
                  data = drop_na(bm_territorial),id = idencuesta,corstr = 'ar1')
    
  }else{
    modelo=geeglm(reformulate(response = i,termlabels = c("edu_imp","sexo_imp","edad_imp_tramo","estrato_disenno",
                                                          "visitas_1ra_c")),
                  data = drop_na(bm_territorial),id = idencuesta,corstr = 'ar1')
    
  }
  
  #names(modelo)<-i
  return(modelo)
})

names(lista_modelos)=variables_imputar




base_modelo=base_modelo%>%
  mutate(across(!!variables_imputar,.fns = ~ifelse(is.na(.x),
                                                   predict(lista_modelos[[rlang::as_name(quo(.x))]]),
                                                   .x)),
         edu_imp=ifelse(is.na(edu_imp),"Media",edu_imp))




saveRDS(base_modelo,"datos/insumos_ponderadores/base_modelo_imp.RDS")
