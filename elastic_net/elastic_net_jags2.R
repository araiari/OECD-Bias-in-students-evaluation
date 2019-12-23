rm(list = ls(all.names = TRUE))
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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
XX=(dati[,-c(1,27)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), a=10, a1 = 10, a2 = 20, prec=0.1, tau = rep(2, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("elastic_net_prior_a.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 50000
thin <-10
param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_EN_prior.Rdata')