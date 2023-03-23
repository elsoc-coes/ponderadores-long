library(tidyverse)

load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")
factores_expansion <- readRDS("datos/insumos_ponderadores/factores_expansion.RDS")%>%
                      select(idencuesta,ola,pd_diseno,pd_nr,muestra)



# GENERAR CELDAS DE AJUSTE ------------------------------------------------



elsoc_preds_imp <- readRDS("modelamiento NR/predicciones/elsoc_preds_imp.RDS")%>%
  left_join(select(elsoc_long_2016_2022,idencuesta,ola),by=c("idencuesta","ola"))%>%
  group_by(ola,estrato_disenno)%>%
  mutate("cat_pred_estrato"=cut(preds_gee,
                        quantile(preds_gee),
                        labels = c("I","II","III","IV"),
                        include.lowest=TRUE))%>%
  group_by(ola,estrato_disenno,cat_pred_estrato)%>%
  mutate(across(responde,~sum(responde)/length(responde),.names = "tasa_resp_estrato"))%>%
  ungroup()


elsoc_nr <- factores_expansion%>%
            left_join(elsoc_preds_imp,by=c("idencuesta","ola"))%>%
            mutate(pd_celdas_estrato=ifelse(muestra==1,
                                    ifelse(ola==1,pd_nr,pd_diseno/tasa_resp_estrato),
                                    ifelse(ola==3,pd_nr,pd_diseno/tasa_resp_estrato)),
                   responde=ifelse(muestra==1,
                                   ifelse(ola==1,1,responde),
                                   ifelse(ola==3,1,responde)))%>%
            select(idencuesta,ola,muestra,responde,pd_diseno,pd_nr,pd_celdas_estrato)


# PODAR LOS PESOS ---------------------------------------------------------

source("generar ponderadores/funciones/funcion para podar los pesos.R")

#CELDAS ESTRATO
elsoc_nr$pd_celdas_estrato_trm=trimpesos_unev(elsoc_nr,5,95,"pd_celdas_estrato")$pd_trimmed
# RAKING ------------------------------------------------------------------

elsoc_long_dv_nr <- elsoc_long_2016_2022%>%
  select(idencuesta,ola, muestra,tipo_atricion,m0_sexo,m0_edad,segmento_disenno)%>%
  mutate( tramo_etario= factor(case_when(
           m0_edad< 30~1,
           m0_edad >29&m0_edad <40~2,
           m0_edad>39& m0_edad < 50~3,
           m0_edad>49 &m0_edad <60~4,
           m0_edad >59 ~5),
           labels = c("18 a 29","30 a 39","40 a 49","50 a 59","60 o más")),
         m0_sexo=factor(m0_sexo,labels = c("Hombre","Mujer")))%>%
  select(-m0_edad)%>%
  left_join(select(elsoc_nr,-muestra),by=c("idencuesta","ola"))%>%
  mutate(ola=car::recode(ola,'1=2016;2=2017;3=2018;4=2019;5=2021;6=2022'))%>%
  drop_na()

source("generar ponderadores/funciones/funcion para re escalar los pesos.R")
source("generar ponderadores/funciones/funcion para hacer raking con los pesos.R")

# PONDERADOR POR MUESTRAS -------------------------------------------------


# CELDAS DE AJUSTE ESTRATO
pesos_celdas_estrato_m1 =rake_pesos(filter(elsoc_long_dv_nr,muestra==1),"pd_celdas_estrato_trm", c(2016,2017,2018,2019,2021,2022))%>%
  mutate(muestra=1)
pesos_celdas_estrato_m1$pd_celdas_estrato_rk_rs = rs_pesos(pesos_celdas_estrato_m1,"pd_rk")

pesos_celdas_estrato_m2 = rake_pesos(filter(elsoc_long_dv_nr,muestra==2),"pd_celdas_estrato_trm", c(2018,2019,2021,2022))%>%
  mutate(muestra=2)
pesos_celdas_estrato_m2$pd_celdas_estrato_rk_rs= rs_pesos(pesos_celdas_estrato_m2,"pd_rk")




elsoc_long_dv_nr=list(elsoc_long_dv_nr,
     select(bind_rows(pesos_celdas_estrato_m1,pesos_celdas_estrato_m2),-pd_rk))%>%
    reduce(left_join,by=c("idencuesta","ola","muestra"))




# PONDERADOR POR PANELES --------------------------------------------------
pesos_panel1 <- rake_pesos(filter(elsoc_long_dv_nr,tipo_atricion==1,muestra==1),"pd_celdas_estrato_trm", c(2016,2017,2018,2019,2021,2022))%>%
  mutate(muestra=1)
pesos_panel1$pd_rk_rs <- rs_pesos(pesos_panel1,"pd_rk")

pesos_panel2 <- rake_pesos(filter(elsoc_long_dv_nr,tipo_atricion==1,muestra==2),"pd_celdas_estrato_trm", c(2018,2019,2021,2022))%>%
  mutate(muestra=2)
pesos_panel2$pd_rk_rs <- rs_pesos(pesos_panel2,"pd_rk")


elsoc_long_dv_nr <-elsoc_long_dv_nr%>%
  left_join(select(bind_rows(pesos_panel1,pesos_panel2),-pd_rk),
            by=c("idencuesta","ola","muestra"))%>%
  rename(pd_celdas_estrato_rk_rs_panel=pd_rk_rs)



master_pesos <- elsoc_long_dv_nr%>%
                select(idencuesta,ola,muestra,segmento_disenno,m0_sexo,tramo_etario,everything())%>%
  rename(ponderadorlong_total=pd_celdas_estrato_rk_rs,
         ponderadorlong_panel=pd_celdas_estrato_rk_rs_panel)%>%
  mutate(ola=car::recode(ola,"2016=1;2017=2;2018=3;2019=4;2021=5;2022=6"))

pesos_longitudinales_elsoc <-master_pesos %>%
                              select(idencuesta,ola,starts_with("ponderador"))

write_csv(master_pesos,file="generar ponderadores/resultados/master_pesos.csv")

write_csv(pesos_longitudinales_elsoc,file="generar ponderadores/resultados/pesos_longitudinales_elsoc.csv")