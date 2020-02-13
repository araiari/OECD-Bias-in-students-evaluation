#ELASTIC NET MODELLO GERARCHICO UNIVARIATO.
#OBIETTIVO:SELEZIONARE LE COVARIATE SCUOLA
#LA MATRICE DISEGNO X CONTIENE LE COVARIATE SCUOLA
#LA MATRICE DISEGNO z CONTIENE LE COVARIATE STUDENTE
#IL PRIMO METODO CONSIDERA LE COVARIATE STUDENTE SELEZIONATE IN PRECEDENZA (13 IN TOTALE, POI IN REALTA' LEARNING TIME MATH è STATA TOLTA)
#IL SECONDO MODELLO NON CONSIDERA NESSUNA COVARIATA STUDENTE

#I RISULTATI DICONO DI TENERE SOLO ISCED_ORIENT
#workspace risultati: en_hier_13_stud, en_hier_0_stud 







#PRIMO MODELLO
rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)#la i-esima componenete è la scuola dello studente i
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


ZZ=as.matrix(dati[,c(6,7,8,9,11,12,13,14,15,16,19,22,23)])#covariate studente
Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

for (i in 1:dim(ZZ)[1]){
  for(j in 1:dim(ZZ)[2]){
    Z[i,j]=as.numeric(ZZ[i,j])
  }
}

p = dim(X)[2]
v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    Z = as.matrix(Z))

inits = function() {
  list(#theta0 = 0.0, 
       theta = rep(0,p),
       gamma0 = rep(0,M),
       gamma = matrix(0,M,v),
       a1 = 10, 
       a2 = 20,
       prec = 0.1,
       prec_gamma=rep(0.1, v),
       tau = rep(2, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("elastic_net_hierarchical_ari_plus_no_inter_unif.bug",
                 data = data_JAGS_EN,
                 n.adapt = 60000,
                 inits = inits,
                 n.chains = 1) 

nit <- 40000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
param <- c( "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='EN_hier_13stud.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()


quantili=as.matrix(summary(output)$quantiles)
CI_beta=quantili[16:22,c(1,5)]


idx_cov_BEN = NULL

nomi=colnames(XX)
for(l in 1:p){
  if(CI_beta[l,1]<0 && CI_beta[l,2]>0)
  {
    cat("*** variable ", nomi[l], " excluded \n")
  }
  else
  {
    cat("*** variable ", nomi[l], " included \n")
    idx_cov_BEN = c(idx_cov_BEN, l)
  }
  
}













#SECONDO MODELLO
rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


#ZZ=as.matrix(dati[,c(7,8,9,14,15,16)])
#Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

#for (i in 1:dim(ZZ)[1]){
#  for(j in 1:dim(ZZ)[2]){
#    Z[i,j]=as.numeric(ZZ[i,j])
#  }
#}

p = dim(X)[2]
#v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
#                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X)
 #                  , Z = as.matrix(Z)
)

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
#    gamma = matrix(0,M,v),
    a1 = 10, 
    a2 = 20,
    prec = 0.1,
#    prec_gamma=rep(0.1, v),
    tau = rep(2, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("elastic_net_hierarchical_ari_plus_no_inter_unif_nostud.bug",
                   data = data_JAGS_EN,
                   n.adapt = 60000,
                   inits = inits,
                   n.chains = 1) 

nit <- 40000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
param <- c( "theta",'a1','a2')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='EN_hier_0stud.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()


quantili=as.matrix(summary(output)$quantiles)
CI_beta=quantili[3:9,c(1,5)]


idx_cov_BEN = NULL

nomi=colnames(XX)
for(l in 1:p){
  if(CI_beta[l,1]<0 && CI_beta[l,2]>0)
  {
    cat("*** variable ", nomi[l], " excluded \n")
  }
  else
  {
    cat("*** variable ", nomi[l], " included \n")
    idx_cov_BEN = c(idx_cov_BEN, l)
  }
  
}
