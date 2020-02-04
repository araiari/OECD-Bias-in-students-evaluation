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


dati=read.csv('dati_bivariato.csv', header=T)
dati=dati[,-c(1,2,4,6,12,14,15,31,35,37,39)]
dati=dati[,c(1,2,3,4,5,6,7,8,9,10,12,15,16,17,18,19,22,24,25,31,32)]
dati=na.omit(dati)
N = dim(dati)[1]
Y=as.matrix(dati[,c(20,21)]) #controllare quale colonna è la risposta
Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
g = as.factor(dati[,1])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(7)])#covariate scuola
#for (i in 1:dim(XX)[2]){
#  XX[,i]=XX[,i]/sd(XX[,i])
#}

X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


#ZZ=as.matrix(dati[,c(9:19)])#covariate studente
#Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

#for (i in 1:dim(ZZ)[1]){
#  for(j in 1:dim(ZZ)[2]){
#    Z[i,j]=as.numeric(ZZ[i,j])
#  }
#}

p = dim(X)[2]
#v = dim(Z)[2]
zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)
colnames(X)=colnames(XX)
#colnames(Z)=colnames(ZZ)


data_JAGS_EN <-list(N = N, 
#                    p = p, 
#                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    #Z = as.matrix(Z),
                    Id=Id,zero2=zero2,zerop=zerop,Idp=Idp)

inits = function() {
  list(#theta0 = 0.0, 
    #theta = rep(0,p),
    #gamma0 = rep(0,M),
    #gamma = matrix(0,M,v),
    #a1 = 10, 
    #a2 = 10,
    beta1=zerop, beta2=zerop, beta0=zero2,
    sigma1=5,sigma2=5, rho=0.65,
    #prec_gamma=rep(50, v),
     #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("modello_hierarchical_biv_no_stud.bug",
                   data = data_JAGS_EN,
                   n.adapt = 25000,
                   inits = inits,
                   n.chains = 1) 

nit <- 35000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "beta",'a1','a2','prec_gamma')
#param <- c( "beta0","beta1","beta2",'sigma1','rho','gamma0')
param <- c( "beta0","beta1","beta2",'sigma1','sigma2','rho')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='modello_biv_hier_uno.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()


#quantili=as.matrix(summary(output)$quantiles)
#CI_beta=quantili[1:8,c(1,5)]



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


dati=read.csv('dati_bivariato.csv', header=T)
dati=dati[,-c(1,2,4,6,12,14,15,31,35,37,39)]
dati=dati[,c(1,2,3,4,5,6,7,8,9,10,12,15,16,17,18,19,22,24,25,31,32)]
dati=na.omit(dati)
N = dim(dati)[1]
Y=as.matrix(dati[,c(20,21)]) #controllare quale colonna è la risposta
Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
g = as.factor(dati[,1])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(6,7)])#covariate scuola
XX[,1]=(XX[,1]-mean(XX[,1]))/sd(XX[,1])
#for (i in 1:dim(XX)[2]){
#  XX[,i]=XX[,i]/sd(XX[,i])
#}

X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


#ZZ=as.matrix(dati[,c(9:19)])#covariate studente
#Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

#for (i in 1:dim(ZZ)[1]){
#  for(j in 1:dim(ZZ)[2]){
#    Z[i,j]=as.numeric(ZZ[i,j])
#  }
#}

p = dim(X)[2]
#v = dim(Z)[2]
zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)
colnames(X)=colnames(XX)
#colnames(Z)=colnames(ZZ)


data_JAGS_EN <-list(N = N, 
                    #                    p = p, 
                    #                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    #Z = as.matrix(Z),
                    Id=Id,zero2=zero2,zerop=zerop,Idp=Idp)

inits = function() {
  list(#theta0 = 0.0, 
    #theta = rep(0,p),
    #gamma0 = rep(0,M),
    #gamma = matrix(0,M,v),
    #a1 = 10, 
    #a2 = 10,
    beta1=zerop, beta2=zerop, beta0=zero2,
    sigma1=5,sigma2=5, rho=0.65,
    #prec_gamma=rep(50, v),
    #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("modello_hierarchical_biv_no_stud.bug",
                   data = data_JAGS_EN,
                   n.adapt = 25000,
                   inits = inits,
                   n.chains = 1) 

nit <- 35000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "beta",'a1','a2','prec_gamma')
#param <- c( "beta0","beta1","beta2",'sigma1','rho','gamma0')
param <- c( "beta0","beta1","beta2",'sigma1','sigma2','rho')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='modello_biv_hier_due.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()


#quantili=as.matrix(summary(output)$quantiles)
#CI_beta=quantili[1:8,c(1,5)]


