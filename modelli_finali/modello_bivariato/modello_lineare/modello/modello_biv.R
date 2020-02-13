#MODELLI LINEARI BIVARIATI. VENGONO COSTRUITI 8 (+1) MODELLI LINEARI CON RISPOSTA BIVARIATA.
#OGNI MODELLO HA 2/3 COVARIATE IN PIù RISPETTO AL PRECEDENTE
#IL PRIMO MODELLO ERA UNA PROVA
#il modello migliore verrà selezionato tramite crossvalidation


















# rm(list = ls())
# library(rstan)
# library(rjags)
# library(coda)
# 
# # for plots
# library(ggplot2)
# library(tidyr)
# library(dplyr)
# library(purrr)
# library(ggsci)
# require(gplots)
# require(ggpubr)
# set.seed(49)
# dati=read.csv('dati_bivariato.csv', header=T)
# dati=dati[,-c(1:15)]
# dati=na.omit(dati)
# N=dim(dati)[1]
# levels(dati[,20])[1]=as.character(0)
# levels(dati[,20])[2]=as.character(1)
# levels(dati[,21])[1]=as.character(0)
# levels(dati[,21])[2]=as.character(1)
# levels(dati[,22])[1]=as.character(0)
# levels(dati[,22])[2]=as.character(1)
# #N=500
# #dati=dati[sample(1:dim(dati)[1],N),] #per ora consideriamo solo 5000 dati
# Y=as.matrix(dati[,c(27,28)]) #controllare quale colonna è la risposta
# #Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
# #Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
# XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
# XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
# XX=XX[,c(2,4,11,7,14,9,10,1,17,8,16)]
# XX=as.matrix(XX)
# X=matrix(0,dim(XX)[1], dim(XX)[2])
# for (i in 1:dim(XX)[1]){
#   for(j in 1:dim(XX)[2]){
#     X[i,j]=as.numeric(XX[i,j])
#   }
# }
# colnames(X)=colnames(XX)
# p=dim(X)[2]
# 
# zero2=c(0,0)
# zerop=rep(0,p)
# Id=matrix(0,2,2)
# Idp=matrix(0,p,p)
# for (i in 1:p){Idp[i,i]=1}
# Id[1,1]=Id[2,2]=1
# beta_in=matrix(0,p,2)
# 
# ## data to pass to JAGS (see the code in SSVS_probit.bug)
# data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)
# 
# ## A list of initial value for the MCMC algorithm 
# # that WinBUGS will implement
# inits = function() {
#   list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
#        .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
# }
# 
# model=jags.model("modello_biv.bug",
#                  data = data_JAGS_EN,
#                  n.adapt = 5000,
#                  inits = inits,
#                  n.chains = 1) 
# 
# nit <- 5000
# thin <-5
# #param <- c("beta0", "beta",'a1','a2','a')
# #param <- c("beta0", "beta",'a1','a2')
# param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
# ##The command coda.samle() calls jags from R passing the data and initial value just defined
# output <- coda.samples(model = model,
#                        variable.names = param,
#                        n.iter = nit,
#                        thin = thin)
# save.image(file='risultato_modello_biv_uno.Rdata')
# #x11()
# #plot(output,ask=T)
# #dev.off()



















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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,6,11)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_due.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,6,11,7,14)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_tre.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,6,11,7,14,9,10)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_quattro.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,6,11,7,14,9,10,1,17)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_cinque.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,6,11,7,14,9,10,1,17,8,16)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=2,sigma2=2, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 20000,
                 inits = inits,
                 n.chains = 1) 

nit <- 20000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_sei_big.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(16,20,22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,4,11,7,14,9,10,1,17,8,16,6,3)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=85,sigma2=85, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_sette.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(22,24)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,4,11,7,14,9,10,1,18,8,17,6,3,16,20)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=85,sigma2=85, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_otto.Rdata')
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
#Y[,1]=(Y[,1]-mean(Y[,1]))/sd(Y[,1])
#Y[,2]=(Y[,2]-mean(Y[,2]))/sd(Y[,2])
XX=(dati[,-c(27,28,29)]) #togiere la risposta e pv5lang, read, togliere le scjole
XX=XX[,-c(22)]#tolgo quelle tolte dalla EN
XX=XX[,c(2,4,11,7,14,9,10,1,18,8,17,6,3,16,20,15,23)]
XX=as.matrix(XX)
X=matrix(0,dim(XX)[1], dim(XX)[2])
for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}
colnames(X)=colnames(XX)
p=dim(X)[2]

zero2=c(0,0)
zerop=rep(0,p)
Id=matrix(0,2,2)
Idp=matrix(0,p,p)
for (i in 1:p){Idp[i,i]=1}
Id[1,1]=Id[2,2]=1
beta_in=matrix(0,p,2)

## data to pass to JAGS (see the code in SSVS_probit.bug)
data_JAGS_EN <-list(N = N, Y = Y, X = as.matrix(X),Id=Id,Idp=Idp,zero2=zero2,zerop=zerop)

## A list of initial value for the MCMC algorithm 
# that WinBUGS will implement
inits = function() {
  list(beta0 = zero2, sigma1=85,sigma2=85, rho=0.75, #a normal exp(0.1), a big exp(0.01), a small exp(10)
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("modello_biv.bug",
                 data = data_JAGS_EN,
                 n.adapt = 5000,
                 inits = inits,
                 n.chains = 1) 

nit <- 5000
thin <-5
#param <- c("beta0", "beta",'a1','a2','a')
#param <- c("beta0", "beta",'a1','a2')
param <- c("beta0", "beta1", "beta2",'sigma1','sigma2','rho')
##The command coda.samle() calls jags from R passing the data and initial value just defined
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='risultato_modello_biv_nove.Rdata')
#x11()
#plot(output,ask=T)
#dev.off()

