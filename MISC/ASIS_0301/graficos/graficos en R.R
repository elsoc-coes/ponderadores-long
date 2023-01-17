library(readr)
elsoc_long_pesos <- read_csv("elsoc_long_pesos.csv")
elsoc_long_pesos <- sjmisc::set_na(elsoc_long_pesos,na = c(-999,-888,-666))




lapply(c("muestral", "pd_nr","pd_atricion","pd_atricion_trm","ponderador02","ponderadorlong_total"),
       function(i){
         data_group= elsoc_long_pesos%>%
           filter(muestra==1)%>%
           group_by(muestra,ola)%>%
           mutate(prop=as.numeric(c01 %in% c(3,4,5)))
         
         if(i=="muestral"){
           data_group%>%
             select(prop)%>%
             drop_na()%>%
             summarise(prom=round(100*mean(prop),2))%>%
             mutate(pond=i)  
         }else{
           data_group%>%
             select(prop,!!rlang::sym(i))%>%
             drop_na()%>%
             summarise(prom=round(100*weighted.mean(prop, w = !!rlang::sym(i)),2))%>%
             mutate(pond=i)  
           
         }
         
         
         
       })%>%
  bind_rows()%>%
  plot_ly(x=~factor(ola),
          y=~prom,
          color=~pond,
          type="scatter",
          mode="lines+markers")%>%
  layout(title = 'Satisfacción con la democracia <br><sup>Satisfecho o muy satisfecho, muestra original</sup>',
         xaxis = list(title = 'Ola del estudio'),
         yaxis = list(title = 'Porcentaje'))%>%
  htmlwidgets::saveWidget(file="PRESENTACIONES/ASIS_0301/graficos/satis_demo_ponderadores.html",
                          selfcontained = TRUE)
