library(readxl)
library(tidyverse)

ine_sex <- read_excel("DATOS/INE/sexo_edad_urbano.xlsx",
            sheet = "tabla")

# FILTRAR 18 y 75

tot_sexo <- ine_sex%>%
  filter(Edad>17& Edad<76)%>%
  reshape2::melt(id.vars='Edad',measure.vars=c('Hombre','Mujer'))%>%
  group_by(variable)%>%
  summarise(Freq=sum(value))
  
saveRDS(tot_sexo,file='DATOS/INE/tot_sexo.RDS')  
  
