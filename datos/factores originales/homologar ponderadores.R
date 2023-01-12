library(haven)
library(tidyverse)

load("datos/oficiales/ELSOC_Long_2016_2022_v1.00_R.RData")



pond_16 <- read_dta("datos/factores originales/2016/ponderadores_coes.dta")%>%
            rename(idencuesta=id,pd_diseno=W1,pd_nr=W2,pd_region=W3,pd_sexo=W4,pd_elsoc=PonderadorW4)%>%
            select(idencuesta,pd_elsoc,pd_diseno,pd_region,pd_nr,pd_sexo)%>%
            mutate(ola=1,
                   muestra=1,
        #Agregar ID cambiado
                   idencuesta=ifelse(idencuesta==13113012,13113015,idencuesta))

pond_17 <- read_dta("datos/factores originales/2017/factores de expansion ELSOC 2017.dta")%>%
            rename(pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_elsoc=ponderador1)%>%
            select(idencuesta,pd_elsoc,pd_diseno,pd_nr,pd_region,pd_sexo)%>%
            mutate(ola=2,
                   muestra=1)

pond_18_m1 <- read_dta("datos/factores originales/2018/Bases_Factores_Seguimiento_ELSOC2018.dta")%>%
  rename(pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
              mutate(ola=3,
                     muestra=1)
pond_18_m2 <- read_dta("datos/factores originales/2018/Bases_Factores_Refresco_ELSOC2018.dta")%>%
  rename(pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
              mutate(ola=3,
               muestra=2)

pond_18 <- bind_rows(pond_18_m1,pond_18_m2)

pond_19_m1 <- read_dta("datos/factores originales/2019/factores_Seguimiento2016_ELSOC2019.dta")%>%
  rename(idencuesta=folio,pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%            
                mutate(ola=4,
               muestra=1)


pond_19_m2 <- read_dta("datos/factores originales/2019/factores_Seguimiento2018_ELSOC2019.dta")%>%
  rename(idencuesta=folio,pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
              mutate(ola=4,
              muestra=2)
              

pond_19_total <- read_dta("datos/factores originales/2019/factores_Total_ELSOC2019.dta")%>%
                rename(idencuesta=folio,pd_elsoc=ponderador1)%>%
                select(idencuesta,pd_elsoc)

pond_19 <- bind_rows(pond_19_m1,pond_19_m2)%>%
          left_join(pond_19_total,by="idencuesta")


pond_21_m1 <- read_dta("datos/factores originales/2021/factores_M1_ELSOC2020.dta")%>%
  rename(idencuesta=folio,pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
              mutate(ola=5,
              muestra=1)


pond_21_m2 <- read_dta("datos/factores originales/2021/factores_M2_ELSOC2020.dta")%>%
  rename(idencuesta=folio,pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
              mutate(ola=5,
              muestra=2)
              

pond_21_total <- read_dta("datos/factores originales/2021/factores_Total_ELSOC2020.dta")%>%
                rename(idencuesta=folio,pd_elsoc=ponderador1)%>%
                 select(idencuesta,pd_elsoc)

pond_21 <- bind_rows(pond_21_m1,pond_21_m2)%>%
          left_join(pond_21_total,by="idencuesta")




pond_22_m1 <- read_dta("datos/factores originales/2022/factores_M1_ELSOC2022.dta")%>%
  rename(pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
  mutate(ola=6,
         muestra=1)

pond_22_m2 <- read_dta("datos/factores originales/2022/factores_M2_ELSOC2022.dta")%>%
  rename(pd_diseno=W,pd_nr=W1,pd_region=W2,pd_sexo=W3,pd_muestra=ponderador1)%>%
  select(idencuesta, pd_diseno,pd_nr,pd_region,pd_sexo,pd_muestra)%>%
  mutate(ola=6,
         muestra=2)

pond_22_total <- read_dta("datos/factores originales/2022/factores_Total_ELSOC2022.dta")%>%
  rename(pd_elsoc=ponderador1)%>%
  select(idencuesta,pd_elsoc)


pond_22 <- bind_rows(pond_22_m1,pond_22_m2)%>%
  left_join(pond_22_total,by="idencuesta")

elsoc_fe <- bind_rows(pond_16,pond_17,pond_18,pond_19,pond_21,pond_22)


# JUNTAR BASES ------------------------------------------------------------
saveRDS(elsoc_fe,file="datos/insumos_ponderadores/factores_expansion.RDS")



