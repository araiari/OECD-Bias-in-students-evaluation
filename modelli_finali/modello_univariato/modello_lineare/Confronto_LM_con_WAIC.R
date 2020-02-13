# day5_3

# Dai due metodi di covariate selection (SSVS e Elastic Net), abbiamo trovato 
# alcune covariate certamente interessanti, altre certamente da escludere, altre
# invece dubbie. 
# Calcolo del waic dei modelli che le includono nelle varie combinazioni per
# valutare quale è il migliore di essi

rm(list=ls())
set.seed(96)

# Beginning -----------------------------------------------------
library(rjags)
library(coda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr)

setwd('~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_WAIC_vero')
dati <- read.csv('~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/dati_reg.csv')
names(dati)

dati$home_possessions <- (dati$home_possessions-mean(dati$home_possessions))/sd(dati$home_possessions)
dati<-dati[,-1]

# Dall'output di Elastic Net e SSVS, troviamo che le variabili da escludere dal modello
# sono la 5, 8, 9, 11, 18. Dubbia rimane la 22, esclusa dall'EN. Inoltre anche la 3 e 21,
# escluse dalla SSVS

scelte_sempre <- c(1,2,4,6,7,10,12:17,19,20,23:25)
scelte_dubbie <- c(21,3,22)


# modello 1 --- solo le scelte sempre --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[, scelte_sempre])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output1 <- coda.samples(model = model,
                        variable.names = param,
                        n.iter = nit,
                        thin = thin)

save(output1, file='output_WAIC_post1.dat')
rm(output1)

# modello 2 --- tengo solo la 21  --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 21)])

N <- dim(cov)[1]
p <- dim(cov)[2] 

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output2 <- coda.samples(model = model,
                        variable.names = param,
                        n.iter = nit,
                        thin = thin)

save(output2, file='output_WAIC_post2.dat')
rm(output2)

# modello 3 --- tengo solo la 3  --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 3)])

N <- dim(cov)[1]
p <- dim(cov)[2] 

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output3 <- coda.samples(model = model,
                        variable.names = param,
                        n.iter = nit,
                        thin = thin)

save(output3, file='output_WAIC_post3.dat')
rm(output3)

# modello 4 --- tengo solo la 22 --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 22)])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output4  <- coda.samples(model = model,
                         variable.names = param,
                         n.iter = nit,
                         thin = thin)

save(output4, file='output_WAIC_post4.dat')
rm(output4)

# modello 5 --- tengo la 3 e 22  --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre,3, 22)])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output5  <- coda.samples(model = model,
                         variable.names = param,
                         n.iter = nit,
                         thin = thin)

save(output5, file='output_WAIC_post5.dat')
rm(output5)

# modello 6 --- tengo solo le 21 e 3--------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 21, 3)])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output6  <- coda.samples(model = model,
                         variable.names = param,
                         n.iter = nit,
                         thin = thin)

save(output6, file='output_WAIC_post6.dat')
rm(output6)

# modello 7 --- tengo solo la 21 e 22  --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 21,22)])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output7  <- coda.samples(model = model,
                         variable.names = param,
                         n.iter = nit,
                         thin = thin)

save(output7, file='output_WAIC_post7.dat')
rm(output7)

# modello 8 --- tengo tutte: la 21, 22, 3 --------------------------------------
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,c(scelte_sempre, 21,22,3)])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output8  <- coda.samples(model = model,
                         variable.names = param,
                         n.iter = nit,
                         thin = thin)

save(output8, file='output_WAIC_post8.dat')
rm(output8)

# modello extra --- tengo c=scelte ----------------------------
scelte <- c(1:7,10,12:17,23:25)
Y <- dati$pv5math/100
cov <- cbind(intercept=rep(1,length(Y)), dati[,scelte])

N <- dim(cov)[1]
p <- dim(cov)[2]

sreg <- summary(lm(Y~as.matrix(cov[,-1])))
media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p)
rm(sreg)

reg_dati <- list(N = N, 
                 p = p,
                 Y = Y,
                 X = cov,
                 media_beta = media_beta,
                 tau_beta = 1/varianza_beta)

inits = function() {
  list(beta = rep(0,p),
       tau2 = 1/5,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model <- jags.model("regressione_WAIC_jags.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=500)

param <- c("beta", "tau2","lik")

nit <- 10000 
thin <- 5

output_extra <- coda.samples(model = model,
                             variable.names = param,
                             n.iter = nit,
                             thin = thin)

save(output_extra, file='output_WAIC_extra.dat')
rm(output8_extra)



# WAIC --------------------------------------
rm(list=ls())

my_WAIC <- function(output, colonne_lik){
  
  lppd <- sum( apply(output[[1]][, colonne_lik], 2, function(x) log(mean(x)) ) )
  p_waic <- sum( apply(output[[1]][, colonne_lik],2, function(x) var(log(x)) )  )
  
  WAIC   <- lppd - p_waic #dalle slide prof ; -2*lppd + 2*p_WAIC da corradin
  return(WAIC)
}

WAIC <- numeric(8)

load("output_WAIC_post1.dat");  colonne_lik1 <- 19:8972 
WAIC[1] <- my_WAIC(output1, colonne_lik1)
rm(output1)

load("output_WAIC_post2.dat"); colonne_lik2 <- 20:8973
WAIC[2] <- my_WAIC(output2, colonne_lik2)
rm(output2)

load('output_WAIC_post3.dat'); #colonne_lik3 <- colonne_lik2
WAIC[3] <- my_WAIC(output3, colonne_lik2)
rm(output3)

load('output_WAIC_post4.dat'); #colonne_lik4 <- colonne_lik2
WAIC[4] <- my_WAIC(output4, colonne_lik2)
rm(output4)

load("output_WAIC_post5.dat"); colonne_lik5 <- 21:8974
WAIC[5] <- my_WAIC(output5,colonne_lik5)
rm(output5)

load("output_WAIC_post6.dat"); #colonne_lik6 <- 21:8974
WAIC[6] <- my_WAIC(output6, colonne_lik5)
rm(output6)

load("output_WAIC_post7.dat")
WAIC[7] <- my_WAIC(output7, colonne_lik5)
rm(output7)

load("output_WAIC_post8.dat"); colonne_lik8 <- 22:8975
WAIC[8] <- my_WAIC(output8, colonne_lik8)
rm(output8)


colnames(WAIC) = c('sempre','sempre+21','sempre+3',
                   'sempre+22','sempre+22+3',
                   'sempre+21+3', 'sempre+21+22',
                   'sempre+dubbie')
save(WAIC,file='WAIC_post_indici.dat')


load('output_WAIC_extra.dat'); 
WAIC_extra <- my_WAIC(output_extra, 19:8972)
rm(output_extra)

# modello bello -----------------------------
# scegliamo il modell 5 che ha WAIC massimo
# le sue covariate sono:
colnames(dati)[c(1,2,4,6,7,10,12:17,19,20,23:25,3, 22)]
load('output_WAIC_post5.dat')

output5 <- output5[[1]][,1:20]
output5 <- as.mcmc.list(output5)

# analisi di convergenza
x11(); plot(output5)

geweke.diag(output5, frac1=0.1, frac2=0.5)
pnorm(abs(geweke.diag(output5, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
# tutti convergono, il peggiore è beta8 ma è accettabile

x11();acfplot(output5, lag.max = 30)

# plot posteriors (ci mette un po')
library(lattice)
densityplot(output5)