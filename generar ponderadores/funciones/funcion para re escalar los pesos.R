

rs_pesos<- function(base,peso){

  n_ola <- base%>%
    group_by(ola)%>%
    summarise(n=n())
  

  
  suma_ola <- base%>%
    group_by(ola)%>%
    summarise(suma=sum(!!rlang::sym(peso),na.rm = TRUE))
  
  valores_n <- n_ola$n
  names(valores_n)<- n_ola$ola

  n_individuo<- base%>%
    mutate(rw=dplyr::recode(ola, !!!valores_n))%>%
    pull(rw)
  
  valores_suma <- suma_ola$suma
  names(valores_suma)<- suma_ola$ola
  
  suma_individuo <- base%>%
    mutate(rw=dplyr::recode(ola, !!!valores_suma))%>%
    pull(rw)
  
peso <-   n_individuo*getElement(base,peso)/suma_individuo

  return(peso)  
}




