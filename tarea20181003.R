#Tarea 2018/10/03
#Jorge III Altamirano Astorga - 175904
#Regresión Avanzada


#   Carga de librerías ---------

library(tidyverse)
library(R2jags)

#   Funciones generales --------

prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#   Lectura de datos ---------
salarios<-read.table("salarios.txt",header=TRUE)
n<-nrow(salarios)
pairs(salarios)
m <- 3
data<-list("n"=n,"m"=m,
           "y"=salarios$Y,
           "x"=structure(.Data=c(salarios$X1, salarios$X2, salarios$X3),
                         .Dim=c(n,3)),
           "xf"=structure(.Data=c(5.4,6.2,6.4, 17.0,12.0,21.0, 6.0,5.8,6.1),
                          .Dim=c(3,3))
)


#   Gráficas de los datos ---------
par(mfrow=c(2,2))
hist(salarios$Y)
hist(salarios$X1)
hist(salarios$X2)
hist(salarios$X3)
par(mfrow=c(1,1))

# Jags & inits ------
# tiene 2 betas
inits<-function(){list(beta=rep(0,3),tau=1,yf1=rep(0,n),yf2=rep(0,m))}
#-Selecting parameters to monitor-
parameters<-c("beta","tau", "yf2", "sig2")
ej4.jags<-jags(data,inits,parameters,model.file="tarea20181003.txt",
               n.iter=100000,n.chains=2,n.burnin=10000,n.thin=10)

# Outputs de Jags ------------
out<-ej4.jags$BUGSoutput$sims.list
z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)


# Beta's -----
z<-out$beta
pairs(z)

# Información del modelo ----
out.sum<-ej4.jags$BUGSoutput$summary
out.sum.t<-out.sum[grep("beta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$beta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

# Predicciones y2: Sumario-----
ej4.jags$BUGSoutput$sims.list$yf2  %>% pairs
ej4.jags$BUGSoutput$sims.list$yf2 %>% summary
### Tomado de https://stackoverflow.com/a/36066450/7323086
# posterior_means <- 
posterior_means <- out.sum.t[,1]
str(posterior_means)

#--- Predicciones -----
posterior_means %*% data$xf 

# Datos input y2 ------
data$xf
