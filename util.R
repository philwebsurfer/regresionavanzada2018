#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

exploracionMCMC <- function(modelo){
  if("BUGSoutput" %in% names(modelo)){
    modelo <- modelo$BUGSoutput
  }
  out <- modelo$sims.list
  z <- out$beta[,2]
  par(mfrow=c(2,2))
  plot(z,type="l")
  plot(cumsum(z)/(1:length(z)),type="l")
  hist(z,freq=FALSE)
  acf(z)
  par(mfrow=c(2,2))
}

exploracionMCMC_r <- function(modelo){
  if("BUGSoutput" %in% names(modelo)){
    modelo <- modelo$BUGSoutput
  }
  out <- modelo$sims.list
  z <- out$r
  par(mfrow=c(2,2))
  plot(z,type="l")
  plot(cumsum(z)/(1:length(z)),type="l")
  hist(z,freq=FALSE)
  acf(z)
  par(mfrow=c(2,2))
}

betasMCMC <- function(modelo){
  if("BUGSoutput" %in% names(modelo)){
    modelo <- modelo$BUGSoutput
  }
  out <- modelo$sims.list
  out.sum<-modelo$summary
  out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
  out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
  dimnames(out.sum.t)[[2]][4]<-"prob"
  print(out.sum.t)
}



#DIC
dicMCMC <- function(modelo){
  if("BUGSoutput" %in% names(modelo)){
    modelo <- modelo$BUGSoutput
  }
  print(modelo$DIC)
}

#Pseudo R2
# R2<-(cor(calif$SP,out.yf[,1]))^2
R2 <- function(x, modelo){
  if("BUGSoutput" %in% names(modelo)){
    modelo <- modelo$BUGSoutput
  }
  out.sum<-modelo$summary
  out.yf<-out.sum[grep("yf",rownames(out.sum)),]
  print(cor(x, out.yf[,1])^2)
}
