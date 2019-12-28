# day 7_1

# confronto il modello lineare con il modello lineare a effetti misti
# utilizzo il criterio di massimizzazione dell'indice WAIC

# Beginning ---------------------------------------------------------------------------
rm(list=ls())

# load del dataset pronto
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/dati_hierarchical")
load('dati_confronto_LM_LMM_scuola.RData')
names(dati)
dati <- dati[,-1] # tolgo lo student id che non mi serve

ngr <- length(unique(dati$school_id)) #numero di scuole
N <- dim(dati)[1] # numero studenti
p <- dim(dati)[2]-1-1 # tolgo gruppo e voto e id_scuola



# Preparazione del dataset (oppure load del dataset pronto)
dati <- read.csv("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/dati_sistemati_E_sistemazione_dati/data_tidy.csv")

names(dati)

# colonne dummies: educazione dei genitori ALTA se laureati, MEDIA se diplomati
edu_parent_high <- as.factor(as.character(dati$highest_education_parents))
levels(edu_parent_high) <- c('0','0','0','0','1','1','0')
edu_parent_high <- as.numeric(edu_parent_high)-1
edu_parent_medium <- as.factor(as.character(dati$highest_education_parents))
levels(edu_parent_medium) <- c('0','0','1','1','0','0','0')
edu_parent_medium <- as.numeric(edu_parent_medium)-1

# video games: 1 se li usano, 0 se non li usano o non li hanno
levels(dati$video_games) <- c('No','Yes','No')
dati$video_games <- as.numeric(dati$video_games)

# immigrants: 1 se sono immigrati, 2 se sono italiani doc
levels(dati$index_immigration_status) <- c('Yes','No','Yes')
dati$index_immigration_status <- as.numeric(dati$index_immigration_status)

# tempo di studio misurato in ore e non in minuti
dati$learning_time_lang <- dati$learning_time_lang/60
dati$learning_time_math <- dati$learning_time_math/60
dati$learning_time_scie <- dati$learning_time_science/60

names(dati)

dati <- dati[,c(3,2,37, 8, 9, 16, 17, 20, 22, 23, 25:27, 29, 31, 32, 34, 36, 13:15)]
dati <- cbind(dati, edu_parent_high, edu_parent_medium)

colnames(dati)

# tolgo gli na
dati <- na.omit(dati)
dati$home_possessions <- (dati$home_possessions-mean(dati$home_possessions))/sd(dati$home_possessions)

# elimino righe delle scuole con 1 solo studente
n_scuola <- NULL
lev <- levels(as.factor(as.character(dati$school_id)))
for (i in 1:length(unique(dati$school_id))){
  n_scuola <- c(n_scuola, sum( dati$school_id==lev[i] ) )
}

which(n_scuola <2)
lev <- lev[which(n_scuola <2)]
for (i in 1:length(lev)){
  dati <- dati[-which(dati$school_id==lev[i]),]
}

# trasformo i nomi dei gruppi: scuole da 1 a length(unique(school_id)) 
dati$school_id <- as.factor(as.character(dati$school_id))
levels(dati$school_id) <- as.character(1:length(levels(dati$school_id)))
dati$school_id <- as.numeric(dati$school_id)

# trasformo tutte le cov. continue in numeric
for (i in 1: dim(dati)[2])
  dati[,i] <- as.numeric(dati[,i])

# save(dati, file='dati_confronto_LM_LMM_scuola.RData')

# preparo i valori che serviranno nel sampling
names(dati)
dati <- dati[,-1] # tolgo lo student id che non mi serve

ngr <- length(unique(dati$school_id)) #numero di scuole
N <- dim(dati)[1] # numero studenti
p <- dim(dati)[2]-1-1 # tolgo gruppo e voto e id_scuola


# 1) LMM1 con covariate studente, raggruppati per scuola --------------------------------

# In questo caso, consideriamo il modello seguente:
# i = studente nella j = scuola
# Y_ij =  theta_j + gamma_ij * X_ij + err_ij

# Theta e gamma sono gaussiani di media zero.
# Come prior per la precision dei theta_j e dei gamma_ij utilizziamo un'esponenziale(100) traslata di 0.5

inits1 <- function(p, ngr){
  list(
    theta = rep(0.1, ngr),
    tau2_theta = rep(1, ngr),
    gamma = matrix(0.1, p, ngr),
    tau2_gamma = matrix(1, p, ngr),
    tau2 = 1/100
  )
}

library(rjags)
library(coda)

setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/risultati_WAIC")

model <- jags.model("simple_LMM.bug",
                    data = list(Y = dati[,2],
                                X = dati[,3:dim(dati)[2]], # effetto random
                                gr = dati[,1],
                                p = p,
                                N = N,
                                ngr = ngr),
                    n.adapt = 1000,
                    inits = inits1(p, ngr),
                    n.chains = 1) 

update(model,n.iter=19000)

output <- coda.samples(model = model,
                       variable.names = c("gamma", "theta", 
                                          "tau2", "lik",
                                          'tau2_gamma', 'tau2_theta'),
                       n.iter = 50000,
                       thin = 10)

save(output, file = 'output_TANTI_studenti_RAGGRUPPATI3.dat')

# .. analisi di convergenza -----------------------------------
load('output_TANTI_studenti_RAGGRUPPATI3.dat')
dim(output[[1]])

colonne <- which(colnames(output[[1]])=='theta[1]') : which(colnames(output[[1]])=='theta[449]')
u <- which(runif(ngr, min=0, max=1) <=0.1) + colonne[1]
output_theta <- output[[1]][,u]
output_theta <- mcmc.list(output_theta=output_theta)

colonne2 <- which(colnames(output[[1]])=='gamma[1,1]') : (which(colnames(output[[1]])=='tau2')-1)
u <- which(runif(9160, min=0, max=1) <=0.01) 
output_gamma <- output[[1]][,u]
output_gamma <- mcmc.list(output_gamma=output_gamma)

rm(output)

x11(); plot(output_theta)
# se aumento la varianza, traceplot belli e tutte gaussiane di media zero

geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z
pnorm(abs(geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
sum(pnorm(abs(geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2<=0.05)
# quasi tutti a convergenza

x11();acfplot(output_theta, lag.max = 100)
# un po' troppo sali e scendi

x11(); plot(output_gamma)
# alcune belle, altre con multimodalità, altre ancora non esplorano tutto il supporto

geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z
pval <- pnorm(abs(geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
sum(pval <=0.05)/length(pval)
# quasi tutti a convergenza

x11();acfplot(output_gamma, lag.max = 50)
# alcune ok, altre da buttare

graphics.off()
rm(output_gamma, output_theta, colonne, colonne2,u)

# i theta (corrispondenti all'intercetta diversa nei gruppi)
# risultano spesso multimodali. Questo mi fa pensare che in una scuola ci
# siano studenti molto bravi e altri meno bravi indipendentemente 
# dalla scuola che frequentano

# 2) LM ----------------------------------------------------------------
# Utilizzo il modello lineare classico:
# Y_i = beta * X_i + eps_i

inits2 = function(p) {
  list(beta = rep(0,p+1),
       tau2 = 1/100,
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

sreg <- summary(lm(dati[,2]~as.matrix(dati[, 3:dim(dati)[2]])))

media_beta <- as.numeric(sreg$coefficients[,1])
#varianza_beta <- as.numeric(sreg$coefficients[,2]^2)
varianza_beta = rep(10,p+1)
rm(sreg)

model <- jags.model("regressione_new_jags.bug", 
                    data =list(N = N, 
                               p = p,
                               Y = dati[,2],
                               X = cbind(rep(1,N), dati[,3:dim(dati)[2]]), 
                               media_beta = media_beta,
                               tau_beta = 1/varianza_beta),
                    n.adapt = 1000,
                    inits2aer = inits(p),
                    n.chains = 1) 

update(model, n.iter=19000)

output <- coda.samples(model = model,
                             variable.names =  c("beta", "tau2","lik"),
                             n.iter = 50000,
                             thin = 10)

save(output, file='output_TANTI_studenti_SOLI2.dat')

# .. analisi di convergenza ----------------------------------------------

colonne_lik1 <- which(colnames(output[[1]])=='lik[1]') : (which(colnames(output[[1]])=='tau2')-1)
output <- output[[1]][,-colonne_lik1]
output <- mcmc.list(output=output)

x11(); plot(output, ask=T)
# molto belli i grafici dei trace plot, regolari le posterior distributions

geweke.diag(output, frac1=0.1, frac2=0.5)[[1]]$z
pnorm(abs(geweke.diag(output, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
# convergenza tutti i beta e tau2

x11();acfplot(output, lag.max = 50)
# autocorrelazione va a zero come dovrebbe

# 3) LMM2 nuovo ---------------------------------------------------------------

# In questo caso, aggiungiamo l'intercetta:
# i = studente nella j = scuola
# Y_ij =  theta 0 + theta_j + gamma_ij * X_ij + err_ij

inits3 <- function(p, ngr){
  list(
    theta0 = 0.1,
    tau2_theta0 = 1/10,
    theta = rep(0.1, ngr),
    tau2_theta = rep(1/10, ngr),
    gamma = matrix(0.1, p, ngr),
    tau2_gamma = matrix(1/10, p, ngr),
    tau2 = 1/100
  )
}

library(rjags)
library(coda)

setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/risultati_WAIC")

model <- jags.model("simple_LMM2.bug",
                    data = list(Y = dati[,2],
                                X = dati[,3:dim(dati)[2]], # effetto random
                                gr = dati[,1],
                                p = p,
                                N = N,
                                ngr = ngr),
                    n.adapt = 1000,
                    inits = inits3(p, ngr),
                    n.chains = 1) 

update(model,n.iter=19000)

output <- coda.samples(model = model,
                       variable.names = c("gamma", "theta", 
                                          "theta0",
                                          "tau2", "lik"),
                       n.iter = 50000,
                       thin = 10)

save(output, file = 'output_TANTI_studenti_RAGGRUPPATI_new1.dat')

# .. analisi di convergenza -----------------------------------
load('output_TANTI_studenti_RAGGRUPPATI_new1.dat')
dim(output[[1]])

colonne <- c(which(colnames(output[[1]])=='theta[1]') : which(colnames(output[[1]])=='theta[449]'), 
             which(colnames(output[[1]])=='theta0'))
u <- which(runif(ngr, min=0, max=1) <=0.1) + colonne[1]
output_theta <- output[[1]][,u]
output_theta <- mcmc.list(output_theta=output_theta)

colonne2 <- which(colnames(output[[1]])=='gamma[1,1]') : (which(colnames(output[[1]])=='tau2')-1)
u <- which(runif(9160, min=0, max=1) <=0.01) 
output_gamma <- output[[1]][,u]
output_gamma <- mcmc.list(output_gamma=output_gamma)

x11(); plot(output_theta)
# se aumento la varianza, traceplot belli e tutte gaussiane di media zero

geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z
pnorm(abs(geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
sum(pnorm(abs(geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2<=0.05)

x11();acfplot(output_theta, lag.max = 50)

x11(); plot(output_gamma)

geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z
pval <- pnorm(abs(geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
sum(pval <=0.05)/length(pval)

x11();acfplot(output_gamma, lag.max = 50)

graphics.off()
rm(output_gamma, output_theta, colonne, colonne2,u)


# WAIC -------------------------------------------------------------------

# Calcolando l'indice waic, controllo quale dei due modelli 
# ha capacità predittiva migliore
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/risultati_WAIC")

my_WAIC <- function(output, colonne_lik){
  
  lppd <- sum( apply(output[[1]][, colonne_lik], 2, function(x) log(mean(x)) ) )
  p_waic <- sum( apply(output[[1]][, colonne_lik],2, function(x) var(log(x)) )  )
  
  WAIC   <- lppd - p_waic #dalle slide prof ; -2*lppd + 2*p_WAIC da corradin
  return(WAIC)
}

WAIC <- numeric(3)

load("output_TANTI_studenti_SOLI2.dat")
colonne_lik1 <- which(colnames(output[[1]])=='lik[1]') : (which(colnames(output[[1]])=='tau2')-1)
WAIC[1] <- my_WAIC(output, colonne_lik1)
rm(output)

load("output_TANTI_studenti_RAGGRUPPATI3.dat")
colonne_lik1 <- which(colnames(output[[1]])=='lik[1]') : (which(colnames(output[[1]])=='tau2')-1)
WAIC[2] <- my_WAIC(output, colonne_lik1)
rm(output)

load("output_TANTI_studenti_RAGGRUPPATI_new1.dat")
colonne_lik1 <- which(colnames(output[[1]])=='lik[1]') : (which(colnames(output[[1]])=='tau2')-1)
WAIC[3] <- my_WAIC(output,colonne_lik1)


WAIC <- rbind( WAIC )
colnames(WAIC) <- c('LM', 'LMM1','LMM2')
WAIC
save(WAIC , file='WAIC_confronto_LM_LMM.dat')


