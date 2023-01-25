elsoc_preds_imp <- readRDS("modelamiento NR/predicciones/elsoc_preds_imp.RDS")

load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")

elsoc_merge=elsoc_long_2016_2022%>%
            left_join(elsoc_preds_imp,by=c("idencuesta","ola"))
  
  
elsoc_merge%>%
  ggplot(aes(x=preds_gee,color=factor(m0_sexo)))+
  geom_density()+
  facet_wrap(~ola)




elsoc_merge%>%
  ggplot(aes(x=preds_gee,color=factor(estrato_disenno)))+
  geom_density()+
  facet_wrap(~ola)