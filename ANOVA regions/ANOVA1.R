## anova 1

# Voglio confrontare i voti di matematica e reading nelle tre regioni
# Per questo, costruisco un modello a effetti misti dipendenti dalla regione
# per la risposta bivariata Y = (voto_matematica, voto_italiano).
# Scelgo la media dei voti nel gruppo j come la somma di un'intercetta del gruppo
# thtea0j e l'effetto dovuto alle p covariate dello studente Xi (theta*Xi)
# Il modello è del tipo:

# Y_ij | mu_ij, sigma^2 ~ N_2( mu_ij, S)
# mu_ij = beta0j + beta1*Xi1 + beta2*Xi2 + ... . beta_p*Xip
# beta_1, ..., beta_p ~ N_2( meanp, I_2* tau^2)
# beta01, ..., beta03 ~ N_2( mean0, I_2* omega^2)
# sigma1 ~ Unif(0,1)
# sigma2 ~ Unif(0,1) 
# rho ~ Unif(-1,1)
# S = [sigma1^2, -sigma1*sigma2*rho;
#      -sigma1*sigma2*rho, sigma2^2] *1/det(S)

# Fisso tau^2 = 5, meanp=(0,0)
# Fisso omega^2 = 5, mean0 = valore dell'intercetta per un LM di risposta Y

# Utilizzo JAGS per simulare dal modello. Il file modello_biv_regioni_1.bug 
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
group <- dati[,2]
group <- as.numeric(group)-1 # Campania=1, Lombardia=2, Trentino=3

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

S <- matrix(ncol=2, nrow=2)
S[1,1] <- sigma2_hat^2/det
S[2,2] <- sigma1_hat^2/det
S[1,2] <- S[2,1] <- -sigma1_hat*sigma2_hat*rho_hat/det

mean0 <- c(lm(pv5math ~ escs +  gender + enjoy_science + video_games + learning_time_science +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1],
           lm(pv5read ~ escs + gender + enjoy_science + video_games + learning_time_science +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1])
mean0 <- as.numeric(mean0)

inits <- function(){
  list(#S=S,
    sigma1 = sigma1_hat,
    sigma2 = sigma2_hat,
    rho = rho_hat,
    beta0 = matrix(0, ncol=ngr, nrow=p),
    beta1 = rep(0,q),
    beta2 = rep(0,q),
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill')
}


reg_dati <- list(N = n, 
                 ngr = ngr,
                 Y = dati[,(q+2+1:p)],
                 X = dati[,(2+1:q)],
                 gr = group,
                 zeroq = rep(0,q), 
                 Idq = diag(rep(1,q)),
                 mean0 = mean0,
                 Idp = diag(rep(1,p)))

library(rjags)
library(coda)

model <- jags.model("modello_biv_regioni_1.bug",
                    data = reg_dati,
                    n.adapt = 1000,
                    inits = inits,
                    n.chains = 1) 

update(model, n.iter=1000)

param <- c("beta1", "beta2", "beta0", "sigma1", "sigma2", "rho", "mu1","mu2")


output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = 11000,
                       thin = 5)

save(output, file='anova1_con_science_result.dat')


# Analisi grafica di convergenza
load('anova1_con_science_result.dat')
library(lattice)

colonne_mu <- which(colnames(output[[1]][,])=='mu1[1]'):(which(colnames(output[[1]][,])=='rho')-1)
output1 <- as.mcmc.list( output[[1]][,-colonne_mu] )
mu <- as.mcmc.list( output[[1]][,colonne_mu] )
save(mu, file='anova1_con_science_result_mu.dat')
save(output1, file='anova1_con_science_result.dat')
rm(mu, output,output1)

load('anova1_con_science_result.dat')
x11();plot(output, ask=T) # buona convergenza di tutto

geweke.diag(output, frac1=0.1, frac2=0.5) # tutto bene
pnorm(abs(geweke.diag(output, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2

x11();acfplot(output, lag.max = 50) # chi più chi meno, ma alla fine vanno a zero

graphics.off()

# ANOVA 
# Confronto il coefficiente beta0 nelle diverse regioni
# Ovvero, sto confrontando la componente della media di Y che dipende dalla regione
# è la media indipendentemente dallo studente e le sue covariate.
# ERGO: a pari di covariate, lo studente con voti migliori è nella regione...

load('anova1_con_science_result.dat')
beta0_math <- output[[1]][,c(1,3,5)]
beta0_read <- output[[1]][,c(2,4,6)]

N <- dim(beta0_math)[1]
ngr <- dim(beta0_math)

# math
df <- data.frame(
  region = factor(rep(c('Campania','Lombardia','TrentinoAA'), each=N)),
  intercept = (c(beta0_math[,1], beta0_math[,2], beta0_math[,3]))
)

library(plyr) # media all'interno dei gruppi
mu_math <- ddply(df, "region", summarise, grp.mean=mean(intercept))

CI_math <- ddply(df, "region", summarise, grp.mean=mean(intercept),
                 grp.sd=sd(intercept), CI1_TCL=-1.96*sd(intercept)+mean(intercept),
                 CI2_TCL=1.96*sd(intercept)+mean(intercept), 
                 CI1_quant= quantile(intercept, 0.025),
                 CI2_quant= quantile(intercept, 0.975))
CI_math

x11();ggplot(df, aes(x=intercept, fill=region)) +
  geom_density(alpha=0.3)+
  geom_vline(data=mu_math, aes(xintercept=grp.mean, color=region),
             linetype="dashed")+
  theme(legend.position="top")+
  ggtitle('pv5math in different regions')+xlab('intercept')+ylab('density')

x11();boxplot(intercept ~ region, df, col=c(2,3,4), horizontal = T); grid()
abline(v=0, lwd=1.8, col='grey')

df <- data.frame(
  region = factor(rep(c('Campania','Lombardia','TrentinoAA'), each=N)),
  intercept = (c(beta0_read[,1], beta0_read[,2], beta0_read[,3]))
)


mu_read <- ddply(df, "region", summarise, grp.mean=mean(intercept))
CI_read <- ddply(df, "region", summarise, grp.mean=mean(intercept),
                 grp.sd=sd(intercept), CI1_TCL=-1.96*sd(intercept)+mean(intercept),
                 CI2_TCL=1.96*sd(intercept)+mean(intercept),
                 CI1_quant= quantile(intercept, 0.025),
                 CI2_quant=quantile(intercept, 0.975))
CI_read

x11();boxplot(intercept ~ region, df, col=c(2,3,4), horizontal = T); grid()
abline(v=0, lwd=1.8, col='grey')

x11();ggplot(df, aes(x=intercept, fill=region)) +
  geom_density(alpha=0.3)+
  geom_vline(data=mu_read, aes(xintercept=grp.mean, color=region),
             linetype="dashed")+
  theme(legend.position="top")+
ggtitle('pv5read in different regions')+xlab('intercept')+ylab('density')

rm(output)
graphics.off()

# Confronto la media mu delle Y nelle diverse regioni.
# Sto ora considerando sia le qualità dello studente sia la regione ad influenzare
# la media dei voti. In questo modo, non diamo per scontato che uno studente
# sia bravo o no solo perchè appartiene a una regione, ma consideriamo anche 
# le sue qualità.
# ERGO: a parità di covariate, lo studente migliore è nella regione...
# e, a parità di regione, lo studente migliore ha tali covariate

load('anova1_result_mu.dat')

dim(mu[[1]][,])[2]/2# 4468
mu_math <- as.matrix(mu[[1]][,1:4468])
mu_math <- cbind(
  rowMeans(mu_math[,which(group==1)]),
  rowMeans(mu_math[,which(group==2)]),
  rowMeans(mu_math[,which(group==3)])  )

mu_read <- as.matrix(mu[[1]][,4469:8936])
mu_math <- cbind(
  rowMeans(mu_read[,which(group==1)]),
  rowMeans(mu_read[,which(group==2)]),
  rowMeans(mu_read[,which(group==3)])  )

N <- dim(mu_math)[1]
ngr <- 3

# math
df <- data.frame(
  region = factor(rep(c("Campania","Lombardia","TrentinoAA"), each=N)),
  mu = c(mu_math[,1],mu_math[,2],mu_math[,3])
)

library(plyr) # media all'interno dei gruppi
mu_math_group <- ddply(df, "region", summarise, grp.mean = mean(mu))
mu_math_group

CI_math <- ddply(df, "region", summarise, grp.mean=mean(mu),
                 grp.sd=sd(mu), CI1=-1.96*sd(mu)+mean(mu),
                 CI2=1.96*sd(mu)+mean(mu))
CI_math2 <- ddply(df,"region",summarize, C1= quantile(mu, 0.025),
                          C2= quantile(mu, 0.975))
CI_math2

ggplot(df, aes(x = mu, fill = region)) +
  geom_density(alpha=0.3)+
  geom_vline(data = mu_math_group, aes(xintercept = grp.mean, color = region),
             linetype = "dashed")+
  theme(legend.position = "top")

#read 
df <- data.frame(
  region = factor(rep(c("Campania","Lombardia","TrentinoAA"), each=N)),
  mu = c(mu_read[,1],mu_read[,2],mu_read[,3])
)

mu_read_group <- ddply(df, "region", summarise, grp.mean=mean(mu))
mu_read_group
CI_read <- ddply(df, "region", summarise, grp.mean=mean(mu),
                 grp.sd=sd(mu), CI1=-1.96*sd(mu)+mean(mu),
                 CI2=1.96*sd(mu)+mean(mu))
CI_read2 <- ddply(df,"region",summarize, C1= quantile(mu, 0.025),
                  C2= quantile(mu, 0.975))
CI_read2

ggplot(df, aes(x=mu, fill=region)) +
  geom_density(alpha=0.3)+
  geom_vline(data=mu_read_group, aes(xintercept=grp.mean, color=region),
             linetype="dashed")+
  theme(legend.position="top")

