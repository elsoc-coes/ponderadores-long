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



bestGEE_QICU_m2 <- readRDS("modelamiento NR/modelos/bestGEE_QICU_m2.RDS")
bestGEE_QICU <- readRDS("modelamiento NR/modelos/bestGEE_QICU.RDS")

bestGEE_QICU_estrato <- readRDS("modelamiento NR/modelos/bestGEE_QICU_estrato.RDS")
bestGEE_QICU_estrato_m2 <- readRDS("modelamiento NR/modelos/bestGEE_QICU_estrato_m2.RDS")


# GENERAR PREDICCIONES ----------------------------------------------------
m1$preds_gee <- predict(bestGEE_QICU,newdata = m1,type="response")
m1$preds_gee_estrato <-predict(bestGEE_QICU_estrato,newdata = m1,type="response")

m2$preds_gee <- predict(bestGEE_QICU_m2,newdata = m2,type="response")
m2$preds_gee_estrato <-predict(bestGEE_QICU_estrato_m2,newdata = m2,type="response")

preds_imp <- bind_rows(m1,m2)%>%
       select(idencuesta,ola,responde,preds_gee,preds_gee_estrato)


saveRDS(preds_imp,file="modelamiento NR/predicciones/elsoc_preds_imp.RDS")
