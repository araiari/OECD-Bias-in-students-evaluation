# anova 2

# Voglio confrontare i voti di matematica e reading nelle tre regioni
# Per questo, costruisco un modello a effetti misti dipendenti dalla regione
# per la risposta bivariata Y = (voto_matematica, voto_italiano).
# Scelgo la media dei voti nel gruppo j come un certo parametro thetaj
# Il modello è del tipo:

# Y_ij | mu_ij, S ~ N_2( thetaj, S)
# theta1, ..., theta3 ~ N_2( mean0, I_2* omega^2)
# sigma1 ~ Unif(0,1)
# sigma2 ~ Unif(0,1) 
# rho ~ Unif(-1,1)
# S = [sigma2^2, -sigma1*sigma2*rho;
#      -sigma1*sigma2*rho, sigma1^2] *1/det(S)

# Fisso omega^2 = 5, mean0 = valore dell'intercetta per un LM di risposta Y

# Sto quindi utilizzando il modello ANOVA descritto alla lezione 06/12/19

# Utilizzo JAGS per simulare dal modello. Il file modello_biv_regioni_2.bug 
# contiene il modello precedentemente descritto da passare a JAGS.
# Il dataset di riferimento è dati_bivariato_std, dove compaiono tutte le cov e 
# le risposte standardizzate. Considero come covariate X solo quelle ritenute 
# interessanti dall'ultima analisi con Cross validation di Daniele

rm(list=ls())
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/ANOVA regioni")

dati <- read.csv(file='~/aa POLIMI/AA_Bayesiana/Progetto Pisa/dati_sistemati_E_sistemazione_dati/dati_bivariato_std.csv')
names(dati)

dati <- dati[,c(1,3,16,20,25,21,28,23,24,15,31,32,22,10,41,42)]
dati <- na.omit(dati)
dati <- dati[which(dati$region != '0'),]
dati[,2] <- as.numeric(dati[,2])-1 # Campania=1, Lombardia=2, Trentino=3
group <- dati[,2]

summary(dati)

n <- dim(dati)[1]
p <- 2 # bivariato, con 2 voti
q <- dim(dati)[2]-4 # covariate interessanti
ngr <- length(unique(group))

rho_hat <- cor(dati$pv5math[which(group==1)] ,dati$pv5read[which(group==1)])/3 + 
  cor(dati$pv5math[which(group==2)], dati$pv5read[which(group==2)])/3 + 
  cor(dati$pv5math[which(group==3)], dati$pv5read[which(group==3)])/3
sigma1_hat <- sd(dati$pv5math)
sigma2_hat <- sd(dati$pv5read)
det <- (sigma1_hat^2*sigma2_hat^2)*(1-rho_hat^2)

S <- matrix(ncol=2, nrow=2) #precision matrix
S[1,1] <- sigma2_hat^2/det
S[2,2] <- sigma1_hat^2/det
S[1,2] <- S[2,1] <- -sigma1_hat*sigma2_hat*rho_hat/det

mean0 <- c(lm(pv5math ~ escs +  gender + enjoy_science + video_games + learning_time_math +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1],
           lm(pv5read ~ escs + gender + enjoy_science + video_games + learning_time_math +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1])
mean0 <- as.numeric(mean0)

inits <- function(){
  list(#S=S,
    sigma1 = sigma1_hat,
    sigma2 = sigma2_hat,
    rho = rho_hat,
    theta = matrix(0, ncol=ngr, nrow=p),
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill')
}


reg_dati <- list(N = n, 
                 ngr = ngr,
                 Y = dati[,(q+2+1:p)],
                 gr = group,
                 mean0 = mean0,
                 Idp = diag(rep(1,p)))

library(rjags)
library(coda)

model <- jags.model("modello_biv_regioni_2.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=1000)

param <- c("theta", "sigma1", "sigma2", "rho")


output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = 11000,
                       thin = 5)

save(output, file='anova2_con_science_result.dat')

# Analisi grafica di convergenza #####################
load('anova2_con_science_result.dat')

library(lattice)

x11();plot(output, ask=T) # buona convergenza di tutto

geweke.diag(output, frac1=0.1, frac2=0.5) # tutti pval abbastanza alti
pnorm(abs(geweke.diag(output, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2

x11();acfplot(output, lag.max = 50) #insomma

graphics.off()

# ANOVA graficamente ###################################
# Confronto le medie a posteriori delle Y nelle tre diverse regioni
# Ovvero, confronto se la media dei voti in una regione è maggiore o minore che 
# in un'altra

theta_math <- output[[1]][,c(4,6,8)]
theta_read <- output[[1]][,c(5,7,9)]

N <- dim(theta_math)[1]
ngr <- dim(theta_math)

# math
df <- data.frame(
  region = factor(rep(c('Campania','Lombardia','TrentinoAA'), each=N)),
  intercept = (c(theta_math[,1], theta_math[,2], theta_math[,3]))
)

library(plyr) # media all'interno dei gruppi
mu_math <- ddply(df, "region", summarise, grp.mean=mean(intercept))

CI_math <- ddply(df, "region", summarise, grp.mean=mean(intercept),
                 grp.sd=sd(intercept), CI1_TCL=-1.96*sd(intercept)+mean(intercept),
                 CI2_TCL=1.96*sd(intercept)+mean(intercept), 
                 CI1_quant= quantile(intercept, 0.025),
                  CI2_quant= quantile(intercept, 0.975))
CI_math

library(ggplot2)

x11(); ggplot(df, aes(x=intercept, fill=region)) + #plot delle posterior distr. dei theta
  geom_density(alpha=0.3)+
  geom_vline(data=mu_math, aes(xintercept=grp.mean, color=region),
             linetype="dashed")+
  theme(legend.position="top")+
  ggtitle('pv5math in different regions')+xlab('intercept')+ylab('density')

x11();boxplot(intercept ~ region, df, col=c(2,3,4), horizontal = T); grid()
abline(v=0, lwd=1.5, col='grey')

#read
df <- data.frame(
  region = factor(rep(c('Campania','Lombardia','TrentinoAA'), each=N)),
  intercept = (c(theta_read[,1], theta_read[,2], theta_read[,3]))
)

mu_read <- ddply(df, "region", summarise, grp.mean=mean(intercept))
CI_read <- ddply(df, "region", summarise, grp.mean=mean(intercept),
                 grp.sd=sd(intercept), CI1_TCL=-1.96*sd(intercept)+mean(intercept),
                 CI2_TCL=1.96*sd(intercept)+mean(intercept),
                 CI1_quant= quantile(intercept, 0.025),
                  CI2_quant=quantile(intercept, 0.975))
CI_read

x11();ggplot(df, aes(x=intercept, fill=region)) +
    geom_density(alpha=0.3)+
  geom_vline(data=mu_read, aes(xintercept=grp.mean, color=region),
             linetype="dashed")+
  theme(legend.position="top")+
  ggtitle('pv5read in different regions')+xlab('intercept')+ylab('density')


x11();boxplot(intercept ~ region, df, col=c(2,3,4), horizontal = T); grid()
abline(v=0, lwd=1.5, col='grey')

graphics.off()
