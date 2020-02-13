#elastic net bivariata finalizzata a eliminare alcune covariate studente quando la risposta è bivariata





rm(list = ls())
library(rstan)
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr)
set.seed(49)
dati=read.csv('dati_bivariato.csv', header=T)
dati=dati[,-c(1:15)]
dati=na.omit(dati)
N=dim(dati)[1]
levels(dati[,20])[1]=as.character(0)
levels(dati[,20])[2]=as.character(1)
levels(dati[,21])[1]=as.character(0)
levels(dati[,21])[2]=as.character(1)
levels(dati[,22])[1]=as.character(0)
levels(dati[,22])[2]=as.character(1)
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.matrix(dati[,c(27,28)]) #controllare quale colonna è la risposta
Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]
colnames(X)=colnames(XX)
zero2=c(0,0)
Id=matrix(0,2,2)
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X),Id=Id,zero2=zero2)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2,beta=beta_in, sigma1=50,sigma2=50, a1 = 10, a2 = 20, tau = rep(2, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("elastic_net_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta",'sigma1','sigma2')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_EN_LM_biv.Rdata')

#x11()
#plot(output,ask=T)
#dev.off()


quantili=as.matrix(summary(output)$quantiles)
CI_beta=quantili[1:52,c(1,5)]


idx_cov_BEN = NULL

nomi=colnames(XX)
for(l in 1:p){
  if((CI_beta[l,1]<0 && CI_beta[l,2]>0) && (CI_beta[l+p,1]<0 && CI_beta[l+p,2]>0))
  {
    cat("*** variable ", nomi[l], " excluded \n")
  }
  else
  {
    cat("*** variable ", nomi[l], " included \n")
    idx_cov_BEN = c(idx_cov_BEN, l)
  }
  
}