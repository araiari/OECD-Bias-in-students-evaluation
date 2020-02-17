# Preditctive distribution of the 2-dim vector [pv5math, pv5read]
# 1) when the NEW student is from an OLD school
# 2) when the NEW student is from a new school
# NOTA: ci si deve rifare al modello calcolato in modello_hierarchical_biv.R


rm(list=ls())
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/predittiva")

# preparazione dati  (oppure fai direttamente load('preparazione_dati.RData')) -------

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

p = dim(ZZ)[2] # numero cov scuola
v = dim(XX)[2] # numero cov. studente

# load('preparazione_dati.RData')
#save.image('preparazione_dati.RData')

# controllo convergenza del modello ---------------------------------------------

load("modello_pred_output.dat")
# param <- c( "beta1","beta2",'gamma0', 'gamma1','gamma2',
#             'rho','sigma1','sigma2',
#             'tau2_gamma0 ','tau2_gamma1','tau2_gamma2')

dim(output[[1]][,]) # 2500, 11044

# beta1 -> colonna1 ; beta2 -> colonna2 ;
# gamma0[1] -> colonna 2+1:M ; gamma0[2] -> colonna 2+M+1:M
# gamma1 -> colonna 2+2*M+1:(M*v)
# gamma2 -> colonna 2+2*M+(m*v)+1:(M*v)

coeff <- output[[1]][,1:(2+2*M+M*v*2)]
#x11(); plot(coeff, ask=F) # 5 minuti di plots, tutti belli regolari e grassi

gamma0 <- output[[1]][,3:(2+M*2)]
sum(colMeans(as.matrix(gamma0))<10) # nessuna intercetta strana

S <- output[[1]][,2+2*M+2*M*v+1:3]
x11(); plot(S) # anche questi regolari e grassi


# predizione ----------------------------------------------------------------

# 1) new student from existing school ---------------------------------------
rm(list=ls())
load('preparazione_dati.RData')
load('modello_pred_output.dat')

niter <- 2500

i <- 10 #seleziono studente

Xnew <- as.numeric(dati[i,3:13]) # cov dello studente i scelto
Znew <- as.numeric(dati[i,14]) 

gr <- g[i] #scuola dello studente selezionato

gamma0 <- output[[1]][,1:2]
gamma0[,1] <- output[[1]][, 2+gr] 
gamma0[,2] <- output[[1]][, 2+M+gr]
gamma0 <- as.matrix(gamma0); colnames(gamma0) <- c('gamma0_1','gamma0_2')

gamma1 <- as.matrix(output[[1]][,M + gr +2+M*(1:v)])
colnames(gamma1) <- c('gamma1_1','gamma1_2','gamma1_3','gamma1_4','gamma1_5','gamma1_6',
                      'gamma1_7','gamma1_8','gamma1_9','gamma1_10','gamma1_11')

gamma2 <- as.matrix(output[[1]][,M*(1+v) + gr +2+M*(1:v)])
colnames(gamma2) <- c('gamma2_1','gamma2_2','gamma2_3','gamma2_4','gamma2_5','gamma2_6',
                      'gamma2_7','gamma2_8','gamma2_9','gamma2_10','gamma2_11')

beta1 <- as.numeric(output[[1]][,1])
beta2 <- as.numeric(output[[1]][,2])

sigma1 <- output[[1]][,'sigma1']
sigma2 <- output[[1]][,'sigma2']
rho <- output[[1]][,'rho']

rm(output)

library(mvtnorm)
Y_pred <- matrix(0,niter,2)
for (j in 1:niter){
  
  Y_media <- c(gamma0[j,1] + Znew*beta1[j] + sum(Xnew*gamma1[j,]), 
              gamma0[j,2] + Znew*beta2[j] + sum(Xnew*gamma2[j,]) )
  Sigma <- rbind( c(sigma1[j]^2, rho[j]*sigma2[j]*sigma1[j]),
                  c(rho[j]*sigma2[j]*sigma1[j], sigma2[j]^2))
  Y_pred[j,1:2] <- rmvnorm(1,mean=Y_media,sigma = Sigma)
  
}

# credible intervals 95%
media <- colMeans(Y_pred); media
c(dati$pv5math[i], dati$pv5read[i])

CI <- rbind(CI_math=c(quantile(Y_pred[,1], 0.05),quantile(Y_pred[,1], 0.95)),
  CI_read = c(quantile(Y_pred[,2], 0.05), quantile(Y_pred[,2], 0.95)) ); CI

# istogramma e posterior density
x11(); par(mfrow=c(1,2));hist(Y_pred[,1], main = 'math', xlab='sampled scores',
   prob=T); abline(v=Y[i,1], col='red'); abline(v=CI[1,], lty=2,
  col='red'); hist(Y_pred[,2], main='read', xlab='sampled scores',prob=T); abline(v=Y[i,2],
   col='blue'); abline(v=CI[2,], lty=2,col='blue')


library(ggplot2)
Yplot <- data.frame(  
  eval=c(Y_pred[,1],Y_pred[,2]), 
  subject=factor(c(rep('math',niter),rep('read',niter))) ) 
library(plyr)
mu <- ddply(Yplot, "subject", summarise, grp.mean=mean(eval))
CI <- ddply(Yplot,"subject",summarize, CI=c(quantile(eval,0.025),quantile(eval,0.975)) )

x11(); ggplot(Yplot, aes(x=eval, fill=subject, color=subject)) + #plot delle posterior distr. dei theta
  geom_density(alpha=0.3)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=subject))+
  geom_vline(data=CI, aes(xintercept=CI, color=subject),
             linetype="dashed")+
  theme_minimal()+
  theme(legend.position="top")+
  ggtitle('pv5math for student i')+xlab('evaluation')+ylab('density')

# LPML 

library(mvtnorm)
CPO <- numeric(dim(Y)[1])

for (l in 1:200){
for (j in 1:1000){
  
  Y_media <- c(gamma0[j,1] + Znew*beta1[j] + sum(Xnew*gamma1[j,]), 
               gamma0[j,2] + Znew*beta2[j] + sum(Xnew*gamma2[j,]) )
  Sigma <- rbind( c(sigma1[j]^2, rho[j]*sigma2[j]*sigma1[j]),
                  c(rho[j]*sigma2[j]*sigma1[j], sigma2[j]^2))
  #Y_pred[j,1:2] <- rmvnorm(1,mean=Y_media,sigma = Sigma)
  CPO[l] <- CPO[l] + 1/dmvnorm(Y[l,], mean = Y_media, sigma= Sigma)
  }
  CPO[l] <- 1/( CPO[l]/1000 )
  print(l)
  }
LPML <- prod((CPO))
LPML

# 2) new student from new school ---------------------------------
rm(list=ls())
load('preparazione_dati.RData')
load('modello_pred_output.dat')

niter <- 2500

i <- 10 #seleziono studente

Xnew <- as.numeric(dati[i,3:13]) # cov dello studente i scelto
Znew <- as.numeric(dati[i,14]) 

beta1 <- as.numeric(output[[1]][,1])
beta2 <- as.numeric(output[[1]][,2])

tau0 <- as.numeric(output[[1]][, 2+2*M+2*M*v+4])
tau1 <- as.matrix(output[[1]][,2+2*M+2*M*v+4+1:v])
tau2 <- as.matrix(output[[1]][,2+2*M+2*M*v+4+v+1:v])

sigma1 <- output[[1]][,'sigma1']
sigma2 <- output[[1]][,'sigma2']
rho <- output[[1]][,'rho']

rm(output)

cinc <- colMeans(Y);# zerov <- rep(0,v)
I <- diag(c(1,1)); #Iv <- diag(rep(1,v))
gamma2 <- gamma1 <- numeric(v)
Y_pred <- matrix(0,niter,2)

set.seed(1)
library(mvtnorm) #per gaussiane random
for (j in 1:niter){ # campiono nuovi valori di gamma0
  gamma0 <- rmvnorm(1,mean=cinc, sigma=I*(1/tau0[j]))
  for (k in 1:v){
    gamma1[k] <- rnorm(1,mean=0, sd=sqrt(1/tau1[j]))
    gamma2[k] <- rnorm(1,mean=0, sd=sqrt(1/tau2[j]))
  }

  Y_media <- c(gamma0[1] + Znew*beta1[j] + sum(Xnew*gamma1), 
               gamma0[2] + Znew*beta2[j] + sum(Xnew*gamma2) )
  Sigma <- rbind( c(sigma1[j]^2, rho[j]*sigma2[j]*sigma1[j]),
                  c(rho[j]*sigma2[j]*sigma1[j], sigma2[j]^2))
  Y_pred[j,1:2] <- rmvnorm(1,mean=Y_media,sigma = Sigma)
  
}


# credible intervals 90%
media <- colMeans(Y_pred); media
c(dati$pv5math[i], dati$pv5read[i])

CI <- rbind(CI_math=c(quantile(Y_pred[,1], 0.05),quantile(Y_pred[,1], 0.95)),
            CI_read = c(quantile(Y_pred[,2], 0.05), quantile(Y_pred[,2], 0.95)) ); CI


# istogramma e predictive density
x11(); par(mfrow=c(1,2)); hist(Y_pred[,1], main='math', xlab='evaluation', probability = T);
abline(v=Y[i,1],col='red'); abline(v=CI[1,],col='red',lty=2)
hist(Y_pred[,2], main='read', xlab='evaluation',probability = T);
abline(v=Y[i,2],col='blue');abline(v=CI[1,],col='blue',lty=2)

library(ggplot2)
Yplot <- data.frame(  
  eval=c(Y$math,Y$read), 
  subject=factor(c(rep('math',niter),rep('read',niter))) ) 
library(plyr)
mu <- ddply(Yplot, "subject", summarise, grp.mean=mean(eval))
CI <- ddply(Yplot,"subject",summarize, CI=c(quantile(eval,0.025),quantile(eval,0.975)) )

x11(); ggplot(Yplot, aes(x=eval, fill=subject, color=subject)) + #plot delle posterior distr. dei theta
  geom_density(alpha=0.3)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=subject))+
  geom_vline(data=CI, aes(xintercept=CI, color=subject),
             linetype="dashed")+
  theme(legend.position="top")+
  ggtitle('pv5math for student i')+xlab('evaluation')+ylab('density')+
  theme_minimal()
  

Yplot <- data.frame(Y$math)
mu <- mean(Y$math)
CI <- c(quantile(Y$math,0.025),quantile(Y$math,0.975)) 

x11(); ggplot(Yplot, aes(x=Y.math, color='red', fill='red')) + #plot delle posterior distr. dei theta
  geom_density(alpha=0.3)+
  geom_vline(data=data.frame(mu), aes(xintercept=mu, color='red'))+
  geom_vline(data=data.frame(CI), aes(xintercept=CI, color='red'),
             linetype="dashed")+
  xlab('evaluation')+ylab('density')+
  theme_minimal()

Yplot <- data.frame(Y$read)
mu <- mean(Y$read)
CI <- c(quantile(Y$read,0.025),quantile(Y$read,0.975)) 

x11(); ggplot(Yplot, aes(x=Y.read)) + #plot delle posterior distr. dei theta
  geom_density(alpha=0.3)+
  geom_vline(data=data.frame(mu), aes(xintercept=mu))+
  geom_vline(data=data.frame(CI), aes(xintercept=CI),
             linetype="dashed")+
  xlab('evaluation')+ylab('density')+
  theme_minimal()

