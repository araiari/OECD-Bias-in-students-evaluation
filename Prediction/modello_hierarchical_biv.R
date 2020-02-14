

rm(list = ls())
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/predittiva")

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



dati <- read.csv(file='~/aa POLIMI/AA_Bayesiana/Progetto Pisa/dati_sistemati_E_sistemazione_dati/dati_bivariato.csv')
names(dati)
dati <- dati[,c(1,2,16,20,25,21,28,23,24,15,31,32,22,10,41,42)]
dati <- na.omit(dati)
names(dati)

N <- dim(dati)[1] # numero studenti

Y <- as.matrix(dati[,15:16]) #eval (controllare quale colonna è la risposta)

g <- as.factor(dati[,2])
M <- length(levels(g)) # numero di scuole
levels(g) <- c(1:M)
g <- as.numeric(g)

ZZ <- as.matrix(dati[,14]) #covariata scuola(rinomino ZZ)
#ZZ <- as.numeric(ZZ[1:N,1])

XX <- as.matrix(dati[,3:13])#covariate studente

p = 1 # numero cov scuola
v = dim(XX)[2] # numero cov. studente
N = dim(dati)[1]
zero2 <- c(0,0)

cinc2 <- colMeans(Y)
zerop <- rep(0,p)
Id <- diag(c(1,1))
Idp <- diag(rep(1,p))
beta_in <- matrix(0,p,2)


data_JAGS_EN <-list(N = N, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(XX),
                    Z = as.matrix(ZZ),
                    Id = Id,
                    cinc2 = cinc2,
                    zerop = zerop,
                    Idp = Idp)

inits = function() {
  list(#theta0 = 0.0, 
    #theta = rep(0,p),
    #gamma0 = rep(0,M),
    #gamma = matrix(0,M,v),
    #a1 = 10, 
    #a2 = 10,
    beta1=zerop, beta2=zerop,
    #beta0=zero2,
    sigma1=60,sigma2=60, rho=0.65,
    tau2_gamma0=5,tau2_theta1=0.1,tau2_theta2=0.1,
    
    #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("modello_hierarchical_biv.bug",
                   data = data_JAGS_EN,
                   n.adapt = 5000,
                   inits = inits,
                   n.chains = 1) 

nit <- 25000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "beta",'a1','a2','prec_gamma')
#param <- c( "beta0","beta1","beta2",'sigma1','rho','gamma0')
param <- c( "beta1","beta2",'sigma1','sigma2','rho','gamma0','gamma1','gamma2','tau2_gamma0 ','tau2_gamma1','tau2_gamma2')
#param <- c( 'beta0',"beta1","beta2",'sigma1','sigma2','rho')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod5.Rdata')
save(output,file='modello_pred_output')

#x11()
#plot(output,ask=T)
#dev.off()
#plot(output[[1]][,7200:7277],ask=T)

#quantili=as.matrix(summary(output)$quantiles)
#CI_beta=quantili[1:8,c(1,5)]


