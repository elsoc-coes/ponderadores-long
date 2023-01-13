
library(tidyverse)
library(readxl)

elsoc_long <- read_csv("datos/insumos_ponderadores/elsoc_longwide.csv")



BASE_2016_COD <- read_excel("datos/oficiales/CIT/BASE_2016_COD.xlsx")%>%
  mutate(across(idencuesta,as.numeric))
BASE_2017_COD <- read_excel("datos/oficiales/CIT/BASE_2017_COD.xlsx")
BASE_2018_COD <- read_excel("datos/oficiales/CIT/BASE_2018_COD.xlsx")
BASE_2019_COD <- read_excel("datos/oficiales/CIT/BASE_2019_COD.xlsx")



id_m1 <- unique(elsoc_long[elsoc_long$muestra==1,]$idencuesta)
id_m2 <- unique(elsoc_long[elsoc_long$muestra==2,]$idencuesta) 


ELSOC_CIT_Dataset_2016 <- read_excel("datos/oficiales/CIT/ELSOC_CIT_Dataset_2016.xlsx")


base_m1 <- filter(BASE_2016_COD,idencuesta %in%id_m1)%>%
  left_join(ELSOC_CIT_Dataset_2016,by='idencuesta')

m1_CIT<- base_m1[rep(seq_len(nrow(base_m1)), each = 6), ]%>%
  group_by(idencuesta)%>%
  mutate(base=factor(c(2016:2019,2021,2022)),
         muestra=1)%>%
  rename(ola=base)


base_m2 <- filter(BASE_2018_COD,idencuesta %in%id_m2)


m2_CIT<- base_m2[rep(seq_len(nrow(base_m2)), each = 4), ]%>%
  group_by(idencuesta)%>%
  mutate(base=factor(c(2018,2019,2021,2022)),
         muestra=2)%>%
  rename(ola=base)

CIT<- bind_rows(m1_CIT,m2_CIT)
CIT <-sjmisc::set_na(CIT,na=c(-994,-995,-996))


CIT <- CIT%>%
  mutate(prop_precaria=100*viv_precaria/viviendas,
         prop_hacina=100*hacinamiento/viviendas)%>%
          ungroup()%>%
         mutate(muestra=factor(muestra,labels = c("Original","Refresco")))

data.table::fwrite(CIT,"datos/insumos_ponderadores/bases_CIT.csv")