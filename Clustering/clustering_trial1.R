### CLUSTERING SCUOLE MULTIVARIATO - TRIAL 1 ---------------------------------------------------
#school_size e certified_teacher_prop

# R & STAN are friends!
library(coda)
library(BNPmix)
library(rstan)

# for plots
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr)

dati <- read.csv("dati_clustering_all.csv")

#SIstemazione dataset-----------------------------------------------------------------------
names(dati)
dati <- dati[,c(2,7,22)]

#rimozione NA
dati <- na.omit(dati) #restano 322 scuole

#vediamo come sono i dati
plot(dati[,2:3])

clust_data <- dati[,2:3]
#clust_data[,1] <- sqrt(clust_data[,1]) #per eventuale trasformazione di covariata

#normalizzazione per il multivariato
means <- sapply(clust_data, mean)
sds <- sapply(clust_data, sd)

for(i in 1:2){
  clust_data[,i] <- (clust_data[,i] - means[i])/sds[i]
}
summary(clust_data)

plot(clust_data)


# MULTIVARIATE-------------------------------------------------------------------

#Modello mixture multivariato basato su un Dirichlet Process (se discount = 0, altrimenti Pitman-Yor)
#che ricava la divisione in cluster. Algoritmo slice sampler
model_SLI_mv <- PYdensity(clust_data, 
                          mcmc = list(niter = 8000, nburn = 7000, nupd = 200,
                                      method = "SLI", model = "LS", hyper = F, print_message = T),
                          prior = list(m0 = colMeans(clust_data), k0 = 5, n0 = 6, #k0 = 5, n0 = 6,
                                       Sigma0 = diag(4, 2), strength = 1, discount = 0),
                          output = list(grid = expand.grid(seq(-4, 4, by = 0.2),
                                                           seq(-4, 4, by = 0.2)), out_type = "MEAN"))
summary(model_SLI_mv)
#applicare un certo discount > 0 fa sì che l'algoritmo ci metta di più, specie se è maggiore di 0,2
#anche applicare una strength maggiore fa incrementare la durata del processo.

#comunque, applicare una strength e un discount maggiori fa aumentare il numero medio dei cluster,
#ma sembra che i cluster grandi/significativi restino sempre dello stesso (basso) numero.
#aumentare k0 e n0 (non troppo però quest'ultimo) consente invece di migliorare la convergenza dei
#traceplot

# plots
p <- plot(model_SLI_mv, show_points = T, show_clust = T, dimension = c(1,2))
ggarrange(p, ncol = 1, nrow = 1) #notiamo 2 cluster maggiori

# diagnostic
coda_SLI_mv <- BNPdens2coda(model_SLI_mv)
summary(coda_SLI_mv)
plot(coda_SLI_mv) #con discount = 0 pare esserci convergenza, aumentando il discount otteniamo un
                  #risultano meno soddisfacente
effectiveSize(coda_SLI_mv)

#tentativo con k0 = 20, strength = 10 e discount = 0.15, ottengo sempre 2 cluster maggiori, e la
#convergenza del traceplot ha un peggioramento

#MARGINAL--------------------------------------------------------------------
#stesso modello di prima ma con strategia marginale
model_MAR_mv <- PYdensity(clust_data, 
                          mcmc = list(niter = 8000, nburn = 7000, nupd = 200,
                                      method = "MAR", model = "LS", hyper = F, print_message = T),
                          prior = list(m0 = colMeans(clust_data), k0 = 20, n0 = 6, #k0 = 5, n0 = 6
                                       Sigma0 = diag(4, 2), strength = 10, discount = 0.15),
                          output = list(grid = expand.grid(seq(-4, 4, by = 0.2),
                                                           seq(-4, 4, by = 0.2)), out_type = "MEAN"))
summary(model_MAR_mv)

# plots 

pM <- plot(model_MAR_mv, show_points = T, show_clust = T, dimension = c(1,2))
ggarrange(pM, ncol = 1, nrow = 1) #2 cluster maggiori

# diagnostic

coda_MAR_mv <- BNPdens2coda(model_MAR_mv)
summary(coda_MAR_mv)
plot(coda_MAR_mv) #molto bello
effectiveSize(coda_MAR_mv) #molto bella
#anche producendo un aumento dei cluster medi cambiando i paramteri
#(k0 = 20, strength = 10, discount = 0.15) otteniamo sempre solo 2 cluster significativi


# partitions
partSLI <- partition(model_SLI_mv)
partMAR <- partition(model_MAR_mv)
table(partSLI$partitions[3,], partMAR$partitions[3,]) #risultati uguali o diversi per numero di cluster
#a seconda dei parametri



# MULTIVARIATE - with HP ----------------------------------------------------------------------
# Modello con rilassamento dell'hyperprior
model_SLI_mv_H <- PYdensity(clust_data, 
                            mcmc = list(niter = 8000, nburn = 7000, 
                                        method = "SLI", model = "LS", hyper = T, print_message = T),
                            output = list(grid = expand.grid(seq(-4, 4, by = 0.2),
                                                             seq(-4, 4, by = 0.2)), out_type = "MEAN"))
summary(model_SLI_mv_H)
#dà errore: può essere conseguenza dei dati che sono molto concentrati su alcuni valori

# MARGINAL - with HP---------------------------------------------
model_MAR_mv_H <- PYdensity(clust_data, 
                            mcmc = list(niter = 4000, nburn = 3000, 
                                        method = "MAR", model = "LS", hyper = T, print_message = T),
                            output = list(grid = expand.grid(seq(-4, 4, by = 0.2),
                                                             seq(-4, 4, by = 0.2)), out_type = "MEAN"))
summary(model_MAR_mv_H)
#dà errore allo stesso modo

