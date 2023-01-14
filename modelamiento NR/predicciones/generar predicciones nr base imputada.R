library(tidyverse)
library(gee)
library(geepack)

# CARGAR MODELOS
base_modelo_imp <- readRDS("datos/insumos_ponderadores/base_modelo_imp.RDS")%>%
                  mutate(ola_fac=factor(ola_fac))



m1 <- filter(base_modelo_imp,muestra==1)%>%
  select(-zona,-ciudad,-comuna)%>%
  drop_na()

m2 <- filter(base_modelo_imp,muestra==0)%>%
  select(-zona,-ciudad,-comuna)%>%
  drop_na()



bestGEE_QICU <- readRDS("MODELOS/GEE/bestGEE_QICU.RDS")
bestGEE_QICU_m2 <- readRDS("MODELOS/GEE/bestGEE_QICU_m2.RDS")



# GENERAR PREDICCIONES ----------------------------------------------------
m1$preds_gee <- predict(bestGEE_QICU,newdata = m1,type="response")


m2$preds_gee <- predict(bestGEE_QICU_m2,newdata = m2,type="response")

preds_imp <- bind_rows(m1,m2)%>%
        select(idencuesta,ola,preds_gee)


saveRDS(preds_imp,file="PONDERADOR/PREDICCIONES NR/elsoc_preds_imp.RDS")
