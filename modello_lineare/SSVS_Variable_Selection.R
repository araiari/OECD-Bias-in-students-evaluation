# dai 3
# SSVS REGRESSION
# Tento di fare selezione delle variabili con il metodo della SSVS
# utilizzando iperparametri diversi per avere risultati più o meno selettivi

# Beginning -------------------------------------------------------------

# Carico i pacchetti
library(rjags)
library(coda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
require(gplots)
require(ggpubr)

# Preparo i dati per la SSVS
setwd('~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base')
dati <- read.csv('dati_reg.csv')
names(dati)

dati<- dati[,-1] # per provare, tengo TUTTE le continue
names(dati)
summary(dati)

dati[,3] <- (dati[,3]-mean(dati[,3]))/sd(dati[,3]) # normalizzo home possession

p <- dim(dati)[2]
N <- dim(dati)[1]
cov <- cbind(rep(1,N), dati[,1:p-1]) 
c_ss <- 500
intersect <- 0.025
# c=100 intersect=0.05 per output3
# c=100, intersect=0.1 per output2
# c=100, intersect=0.25 per output4
# c=100, intersect=0.01 per output5
# c=500, intersect=0.05 per output6 
# c=500, intersect=0.025 per output7
tau_ss <- intersect / sqrt(2 * log(c_ss) * c_ss^2/(c_ss^2 - 1))
c(c_ss*tau_ss,tau_ss)

(tau_ss^2*c_ss^2)
(tau_ss^2)

# SSVS with JAGS --------------------------------------------

reg_dati <- list(N = dim(dati)[1], 
                 p = dim(dati)[2],
                 Y = dati[,p]/100, 
                 X = cov,
                 theta = 0.5,
                 c = c_ss,
                 tau = tau_ss)

inits = function() {
  list(beta = rep(0,p),
       t_Y = 1/5,
       gamma = rep(0,p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model=jags.model("try_SSVS_jags.bug",
                 data = reg_dati,
                 n.adapt = 1000,
                 inits = inits,
                 n.chains = 1) 

update(model,n.iter=500)

param <- c("beta", "gamma", "mdl", "t_Y")

nit <- 50000
thin <-10

output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)

setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS")
save.image(file="output_jags7.RData")

# Analisi dei risultati -------------------------------------------------
# carico tutti i risultati ottenuti, verifico la bontà del modello e la convergenza
rm(list=ls())

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags2.RData")
output2 <- output # quello di marco, con spike più ampia 
model2 <- model   # c=100, intersect=0.1

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags3.RData")
output3 <- output # il mio primo, con valori corrado
model3 <- model   # c=100, intersect=0.05

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags4.RData")
output4 <- output # primo tentativo a caso ---> il più conservativo
model4 <- model   # c=100, intersect=0.25

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags5.RData")
output5 <- output # secondo tentativo a caso --> il più buonista
model5 <- model   # c=100, intersect=0.01

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags6.RData")
output6 <- output # quello di marco, con spike più ampia 
model6<- model   # c=100, intersect=0.1

load("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LM-base/risultati_SSVS/output_jags7.RData")
output7 <- output # quello di marco, con spike più ampia 
model7<- model   # c=100, intersect=0.1

rm(model, output, c_ss, intersect, N, nit, p, tau_ss, thin, inits)
rm(reg_dati,param,sreg)

# Trace plot e plot della Posterior
x11(); plot(output2,ask=T)
dev.off()
#+ output3,2 -> ok (but beta[1] and few others have thin trace plots)
# output5, output4 -> very thin trace plots, multimodal density for some betas
# output6 -> perfect
# output7 -> some covariates don't explore the whole support (thin trace plots)

# Criterio di convergenza di geweke 
pnorm(abs(geweke.diag(output7)[[1]]$z),lower.tail=FALSE)*2
# output2, output3, output4 -> beta converge, gamma not every (some NaN)
# output5 -> more NaN for gamma, pval per betas smaller than before
# output6 -> all beta converge, some gamma p-val = NaN
# output7 -> all beta converge (second best convergence), some gamma p-val = NaN

# Autocorrelation plot
x11();acfplot(output7[[1]][,1:25], lag.max =100)
# output2 -> beta[1], beta[4] a bit autocorr
# output3,4 -> beta[1], beta[4], beta[7] a bit autocorr
# output6 -> beta[1], beta[7], beta[25] a bit autocorr
# output7 -> beta[1], beta[7], beta[25] very autocorr (at the beginning, then ok)

# Summary dell'output
summary(output6)

# Variable selection thecniques ------------------------------
# 1) The median probability moodel (MPM) --------------------------

head(output6) # per contare quali colonne sono i gamma

post_g_6 <-as.matrix(output6[,27:52]) # per output 6
post_mean_g_6 <- apply(post_g_6,2,"mean") 
p6 <- data.frame(value = post_mean_g_6, var = colnames(cov)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("From output 6") 
x11();p6

post_g_5 <-as.matrix(output5[,27:52]) # per output 5
post_mean_g_5 <- apply(post_g_5,2,"mean") 
p5 <- data.frame(value = post_mean_g_5, var = colnames(cov)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("From output 4") 
x11();p5

post_g_4 <-as.matrix(output4[,27:52]) # per output 4
post_mean_g_4 <- apply(post_g_4,2,"mean")
p4 <- data.frame(value = post_mean_g_4, var = colnames(cov)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("From output 4") 
x11();p4

post_g_3 <-as.matrix(output3[,27:52]) # per output 3
post_mean_g_3 <- apply(post_g_3,2,"mean") 
p3 <- data.frame(value = post_mean_g_3, var = colnames(cov)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("From Output 3")
x11();p3

post_g_2 <-as.matrix(output3[,27:52]) # per output 2
post_mean_g_2 <- apply(post_g_2,2,"mean") 
p2 <- data.frame(value = post_mean_g_2, var = colnames(cov)) %>%
  ggplot(aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position="none") + 
  ylab("posterior inclusion probabilities") + 
  xlab("From output 2 (conservativo)") 
x11();p2

# con quanta probabilità sono le covariate che abbiamo preso
colnames(cov)[mp_SSV6]
mp_SSV6 <- as.vector(which(post_mean_g_6 > 0.5))
post_mean_g_6[mp_SSV6]
mp_SSV5 <- as.vector(which(post_mean_g_5 > 0.5))
post_mean_g_5[mp_SSV5]
mp_SSV4 <- as.vector(which(post_mean_g_4 > 0.5))
post_mean_g_4[mp_SSV4]
mp_SSV3 <- as.vector(which(post_mean_g_3 > 0.5))
post_mean_g_3[mp_SSV3]
mp_SSV2 <- as.vector(which(post_mean_g_2 > 0.5))
post_mean_g_2[mp_SSV2]

# confronto tra covatiate USANDO DIVERSE INTERCETTE
mp_SSV2; mp_SSV3; mp_SSV4; mp_SSV5; mp_SSV6

# 2) HPD ---------------------------------------------------------
# Highest posterior density model (HPD)

# Recall that we have represented the model index using a binary coding as 
# mdl=1+2^g1+...+2^gp. The visited model are saved in the chain

### How many models have been visited by the posterior chain:
output <- as.matrix(output6)
length(unique( output[,"mdl"])) #96 modelli in 5. 102 modelli in 4, 117 in 6

## Now we compute the posterior frequency of the visited  models
visited_models<-table(output[,"mdl"])

# getting the unique profiles and sort the results
post_g<- post_g_6
unique_model <- unique(post_g, MARGIN  = 1)
freq <- apply(unique_model, 1, function(b) sum(apply(post_g, MARGIN = 1, function(a) all(a == b))))
#cbind(unique_model[order(freq,decreasing = T),], sort(freq,decreasing = T))

# the HPD model is 
colnames(cov)[as.logical(unique_model[which.max(freq),])]
HDP_SSV6<- c(1:26)[as.logical(unique_model[which.max(freq),])]
HDP_SSV5 <- c(1:26)[as.logical(unique_model[which.max(freq),])]
HDP_SSV4 <- c(1:26)[as.logical(unique_model[which.max(freq),])]

# confronto modelli usando diverse variable selection
HDP_SSV4; HDP_SSV5;mp_SSV2; mp_SSV3; mp_SSV4; mp_SSV5
HDP_SSV6; mp_SSV6

# 3) Credible Intervals --------------------
# Scegliamo di utilizzare questo metodo di scelta per coerenza.
# Infatti, facciamo anche modello penalizzato Elastic Net e anche in esso usiamo CI

beta <- as.matrix(output6[[1]][,1:26]) # estraggo i beta

# 95% posterior credible interval per i beta
CI_beta = apply(beta, 2, quantile, c(0.01275, 0.9725)) 
CI_beta

# ciclo for: quali covariate tenere in base all'appartenenza di 0 al CI 
idx_cov = NULL 
for(l in 1:26){
  if(CI_beta[1,l]<0 && CI_beta[2,l]>0)
  {
    cat("*** variable ", colnames(cov)[l], colnames(beta)[l], " excluded \n")
  }
  # else
  # {
  #   cat("*** variable ", colnames(cov)[l], colnames(beta)[l], " included \n")
  #   idx_cov = c(idx_cov, l)
  # }
  
}

mean_beta_post <- apply(beta, 2, "mean")
mean_beta_post

# Plot dei CI
require(gplots)
plotCI(x = 1:p, y = mean_beta_post, liw = (-CI_beta[1,] + mean_beta_post),  
       uiw = (CI_beta[2,]- mean_beta_post),
       type = "n", lwd = 1.5, main="Decision intervals for HS", ylab = "", xlab = "")
abline(h = 0, col = "blue")


