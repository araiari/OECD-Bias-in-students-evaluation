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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,15,18,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM1.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()










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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,15,17,18,20,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM2.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()









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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM3.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()













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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,7,8,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM4.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM5.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM6.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()








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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
#XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM7.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()











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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,15,18,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LMig1.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()










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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,15,17,18,20,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LMig2.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()









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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM3ig.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()













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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,7,8,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM4ig.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM5ig.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM6ig.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()








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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
#XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM7ig.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()




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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,15,18,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LMig1_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()










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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,15,17,18,20,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LMig2_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()









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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,7,8,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM3ig_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()













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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,7,8,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM4ig_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM5ig_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()














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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
#XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM6ig_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()








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
dati=read.csv('dati_reg.csv', header=T)
N=dim(dati)[1]
#N=500
#dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
Y=as.vector(dati[,27]) #controllare quale colonna è la risposta
Y=(Y-mean(Y))/sd(Y)
XX=(dati[,c(2,3,5,6,7,8,11,13,14,15,16,17,18,20,21,23,24,25,26)]) #togiere la risposta e pv5lang, read, togliere le scjole e le varaibili escluse
#XX=(dati[,c(2,5,6,7,8,11,13,14,15,17,18,20,21,23,24,25,26)])
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
p=dim(X)[2]

colnames(X)=colnames(XX)




## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, p = p, Y = Y, X = as.matrix(X))

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = 0.0, beta = rep(0,p), sigma=2.5, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_lm_invg2.bug",
                 data = data_JAGS_EN,
                 n.adapt = 10000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-10
#param <- c("beta0", "beta",'a1','a2','a')
param <- c("beta0", "beta",'sigma')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='LM7ig_bis.Rdata')
#save(output,file='EN_jags1_a_normal.dat') 
#load('EN_jags1_a_normal.dat')
#x11()
#plot(output,ask=T)
#dev.off()