  library(tidyverse)
  library(gtools)
  library(gee)
  library(geepack)

m1 <- readRDS("datos/insumos_ponderadores/m1.RDS")%>%
      mutate(ola_fac=factor(ola_fac))%>%
      drop_na()


m2 <- readRDS("datos/insumos_ponderadores/m2.RDS")%>%
  select(-zona,-ciudad,-comuna)%>%
  mutate(ola_fac=factor(ola_fac))%>%
  drop_na()

covariables <- c("ola_fac","sexo_imp","estrato_disenno","edad_imp_tramo","edu_imp","civil_imp",
                "nsnr_prom_mod_1ra_c","visitas_lag_c","densidad_pob_km2_c","jh_esc_m_c",
                "prop_hacina_c","tamagno_hog_c","cant_pers_prom_c","prop_precaria_c")


qic_elsoc=function (object, ..., tol = .Machine$double.eps, env = parent.frame()) 
{
  if (!("geeglm" %in% class(object))) {
    stop("QIC requires a geeglm object as input")
  }
  invert <- if ("MASS" %in% loadedNamespaces()) {
    MASS::ginv
  }
  else {
    solve
  }
  computeqic <- function(object) {
    mu <- object$fitted.values
    y <- object$y
    type <- family(object)$family
    quasi <- switch(type, poisson = sum((y * log(mu)) - mu), 
                    gaussian = sum(((y - mu)^2)/-2), binomial = sum(y * 
                                                                      log(mu/(1 - mu)) + log(1 - mu)), Gamma = sum(-y/(mu - 
                                                                                                                         log(mu))), stop("Error: distribution not recognized"))
    object$call$corstr <- "independence"
    object$call$zcor <- NULL
    object$call$formula <-object$formula
    base <- object$data
    object$call$data<- base
    
    model.indep <- eval(object$call, envir = env)
    AIinverse <- invert(model.indep$geese$vbeta.naiv, tol = tol)
    Vr <- object$geese$vbeta
    trace <- sum(diag(AIinverse %*% Vr))
    params <- length(coef(object))
    kpm <- params + length(object$geese$alpha)
    QIC <- -2 * (quasi - trace)
    QICu <- -2 * (quasi - params)
    QICC <- QIC + (2 * kpm * (kpm + 1))/(length(unique(object$id)) - 
                                           kpm - 1)
    output <- c(QIC, QICu, quasi, trace, params, QICC)
    names(output) <- c("QIC", "QICu", "Quasi Lik", "CIC", 
                       "params", "QICC")
    output
  }
  if (length(list(...))) {
    results <- lapply(list(object, ...), computeqic)
    check <- sapply(list(object, ...), function(x) {
      length(x$y)
    })
    if (any(check != check[1])) 
      warning("models are not all fitted to the same number of observations")
    res <- do.call("rbind", results)
    Call <- match.call()
    Call$k <- NULL
    row.names(res) <- as.character(Call[-1L])
    res
  }
  else {
    computeqic(object)
  }
}







back_gee <-function(base,vars,metrica,fijas=NULL){
  
  name_covs <- vars
  
  while(TRUE){
    modelo_all <- geeglm(reformulate(response = "responde",name_covs),
                         family = binomial,
                         id = idencuesta,
                         corstr = 'ar1',
                         data=na.omit(base))
    
    combis <- combinations(length(name_covs),length(name_covs)-1,name_covs)
    
    if(is.null(fijas)){
      ecuaciones <- lapply(1:nrow(combis),function(i){reformulate(response="responde",combis[i,])})
      
    }else{
      covariables[!covariables %in% fijas]
      ecuaciones <- lapply(1:nrow(combis),function(i){reformulate(response="responde",c(fijas,combis[i,]))})
    }
    
    
    modelos <- lapply(1:length(ecuaciones),function(j){geeglm(ecuaciones[[j]],
                                                              family = binomial,
                                                              id = idencuesta,
                                                              corstr = 'ar1',
                                                              data=na.omit(base))})  
    
    var_all<- labels(terms(modelo_all$formula))
    
    
    tabla<- tibble("out"=sapply(1:length(modelos),
                                function(i){setdiff(var_all, 
                                            labels(terms(modelos[[i]]$formula)))}))%>%
            mutate(valor=sapply(modelos, function(i){qic_elsoc(i)[metrica]}),
                   dife=qic_elsoc(modelo_all)[metrica]-valor)
    
    
    #SALIDA LOOP
    if(qic_elsoc(modelo_all)[metrica]-min(tabla$valor)<1) {break} 
    else{
      name_covs <-labels(terms(modelos[[which.min(tabla$valor)]]))
    print(list("fuera"=tabla[which.min(tabla$valor),]$out,
               "gain"=tabla[which.min(tabla$valor),]$dife))}  
  }
  
  print(paste("Modelo final: ",modelo_all$formula)[3])
  return(modelo_all)
  
}




bestGEE_QICU<- back_gee(m1,covariables,"QICu")

saveRDS(bestGEE_QICU,file = "modelamiento NR/modelos/bestGEE_QICU.RDS",
     compress = "bzip2")


bestGEE_QICU_m2 <- back_gee(m2,covariables,"QICu")
saveRDS(bestGEE_QICU_m2,
       file = "modelamiento NR/modelos/bestGEE_QICU_m2.RDS",
        compress = "bzip2")





