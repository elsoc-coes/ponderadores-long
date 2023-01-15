
trimpesos<- function(base,percentil,peso){
  dif <- 100-percentil
  
  high<-quantile(getElement(base,peso),seq(0,1,by=.005),na.rm = TRUE)[201-dif]
  low<- quantile(getElement(base,peso),seq(0,1,by=.005),na.rm = TRUE)[1+dif]

pesos_trimmed <- ifelse(getElement(base,peso)>=high,high,ifelse(getElement(base,peso)<=low,low,getElement(base,peso)))  

cat(paste("Pesos cortados en los valores:","\n",names(low),"=",round(low,2),names(high),"=",round(high,2)))  

base$pd_trimmed <- pesos_trimmed
data_out <- select(base,idencuesta,ola, pd_trimmed)

return(data_out)  

}


