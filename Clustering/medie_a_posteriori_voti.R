### MEDIE DELLE SCUOLE-----------------------------------------------------------------------------

dati <- read.csv("dati_grezzi.csv")
names(dati)

#Sistemazione dataset--------------------------------------------------------------------------------
#prendo le colonne di scuola e valutazioni
dati <- dati[,c(2,44)]
head(dati)
summary(dati)
#cambio levels
dati[,1] <- as.factor(as.character(dati[,1]))
length(levels(dati[,1]))
levels(dati[,1]) <- as.character(1:474)
head(dati)

Y <- dati
#divisione voti per 10
Y[,2] <- Y[,2]/10
head(Y)

m = length(unique(Y[,1]))
n<-sv<-ybar<-rep(NA,m) 
for(j in 1:m) 
{ 
  ybar[j]<- mean(Y[Y[,1]==j,2])    #medie per gruppo
  sv[j]<-var(Y[Y[,1]==j,2])        #varianze empiriche in ogni gruppo
  n[j]<-sum(Y[,1]==j)              #numerosità in ogni gruppo (scuola)
}

#le scuole con un elemento solo non possiedono una varianza within, per cui assegniamo un valore di
#default, maggiore del massimo tra le altre varianze
max(na.omit(sv)) #140.3654
sv[which(is.na(sv)==TRUE)]=150
#summary(ybar)

ybar
n
sv

#Grafico dei dati ----------------------------------------------------------------------
x11()
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
hist(ybar,main="",xlab="sample mean")
plot(n,ybar,xlab="sample size",ylab="sample mean")
# LEFT: histogram of the empirical means per school ybar_j
# RIGHT: very extreme sample averages (very small or very large)  
#        tend to be associated with schools with small sample size


# (IPER)PARAMETRI e PRIOR---------------------------------------------------------------------------
# PARAMETERS: (theta_1,...,theta_474, mu, tau^2,sigma^2)
# theta_j=mean in school j, mu= grand mean,
# tau^2 = variance between schools, sigma^2 = variance within school j (costant)
# la prior per theta_1,...,theta_474  è di tipo gerarchico - vedi i lucidi

# PRIOR
### 1/sigma^2 \sim gamma(nu0/2, (nu0*sigma0^2)/2)
### 1/tau^2 \sim gamma(eta0/2, (eta0*tau0^2)/2)
### mu \sim N(mu0,gamma0^2)
#  sigma^2: within-group variance (it is constant in this case, but a more general model
#            with sigma_j^2)
#  tau^2: between-group variance

#IPERPARAMETRI
par(mfrow=c(1,1))
nu0<-1  ; s20<-100   # iperparametri per la prior di sigma^2 = within-group variance
eta0<-1 ; t20<-100 # iperparametri per la prior di tau^2
mu0<-50 ; g20<-25  # IMPORTANT: iperparametri di mu 
#### A priori P(\mu \in (40,60))= 0.94 circa e E(\mu)=50
#### Prior informativa, ricavata da info sul test somministato a tutta la nazione
dev.off()

#Analisi MCMC---------------------------------------------------------------------
# PARAMETERS: (theta_1,...,theta_474, mu, tau^2,sigma^2)
# theta_j=mean in school j, mu= grand mean,
# tau^2 = variance between schools, sigma^2 = variance within school j (costant)
##### starting values of the MC
theta<-ybar
sigma2<-mean(sv)
mu<-mean(theta)
tau2<-var(theta)
###

### setup MCMC
set.seed(1)
S<-5000
#S<-5
THETA<-matrix( nrow=S,ncol=m)
MST<-matrix( nrow=S,ncol=3)
###

### Algoritmo - GIBBS SAMPLER
#impiega circa 8 minuti per S=3000
#impiega circa 14 minuti per S=5000
for(s in 1:S) 
{
  
  # sample new values of the thetas
  for(j in 1:m) 
  {
    vtheta<-1/(n[j]/sigma2+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
  }
  
  #sample new value of sigma2
  nun<-nu0+sum(n)
  ss<-nu0*s20;for(j in 1:m){ss<-ss+sum((Y[Y[,1]==j,2 ]-theta[j])^2)}
  sigma2<-1/rgamma(1,nun/2,ss/2)
  
  #sample a new value of mu
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu)) 
  
  # sample a new value of tau2
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)
  
  #store results
  THETA[s,]<-theta
  MST[s,]<-c(mu,sigma2,tau2)
  
  #avanzamento
  print(paste0("Iter ",s," su ",S))
  
}

### THETA contiene i valori simulati dei 474 theta_i, MST contiene i valori simulati di 
### mu, sigma^2, tau^2
###

mcmc1<-list(THETA=THETA,MST=MST)

#CONTROLLO DEL RISULTATO-------------------------------------------------------------

stationarity.plot<-function(x,...){
  S<-length(x)
  scan<-1:S
  ng<-min( round(S/474),10)
  group<-S*ceiling( ng*scan/S) /ng
  boxplot(x~group,...)               
}

### Check CONVERGENCE of the MC
# Each graph contains 10 boxplots of 500 next iterations
# (dunque 1/10 di tutti i dati simulati) of mu,sigma^2 e tau^2 
# Boxplots are similar: convergence is OK

x11()
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
stationarity.plot(MST[,1],xlab="iteration",ylab=expression(mu))
stationarity.plot(MST[,2],xlab="iteration",ylab=expression(sigma^2))
stationarity.plot(MST[,3],xlab="iteration",ylab=expression(tau^2))

library(coda)
effectiveSize(MST)
# effective sample size of MST are good!
# Good autocorrelation functions
par(mfrow=c(1,3))
acf(MST[,1]) -> a1
acf(MST[,2]) -> a2
acf(MST[,3]) -> a3

# Stima dell'errore Monte Carlo = posterior sd / sqrt(effectivesamplesize) 
#        per mu, sigma^2, tau^2
MCERR<-  apply(MST,2,sd)/sqrt( effectiveSize(MST) )
MCERR

### POSTERIOR MEANS of  mu, sigma^2, tau^2
apply(MST,2,mean)

100*MCERR/apply(MST,2,mean) #sono tutti e 3 minori di 1%, cioè MCerr diviso la media a posteriori è piccolo


# effective sample size of theta_j are OK!
effectiveSize(THETA) -> esTHETA
esTHETA

# Stima dell'errore Monte Carlo dei THETA = posterior sd / sqrt(effectivesamplesize) 
TMCERR<-  apply(THETA,2,sd)/sqrt( effectiveSize(THETA) )
TMCERR


##### MARGINAL POSTERIOR densities of mu, sigma^2, tau^2
x11()
par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
plot(density(MST[,1],adj=2),xlab=expression(mu),main="",lwd=2,
     ylab=expression(paste(italic("p("),mu,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,1],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )
plot(density(MST[,2],adj=2),xlab=expression(sigma^2),main="", lwd=2,
     ylab=expression(paste(italic("p("),sigma^2,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,2],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )
plot(density(MST[,3],adj=2),xlab=expression(tau^2),main="",lwd=2,
     ylab=expression(paste(italic("p("),tau^2,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,3],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )


##### POSTERIOR MEANS of  mu, sigma, tau     
mean((MST[,1]))        #49.06202
mean(sqrt(MST[,2]))    #6.997619
mean(sqrt(MST[,3]))    #5.976973


#SHRINKAGE---------------------------------------------------------------------------------
### Vediamo come le informazioni sono state condivise tra i diversi gruppi: 
###                   Bayesian hierarchical approach
### Se n_j è GRANDE, la media empirica è una buona stima, anche da punto di vista bayesiano;
### dunque non c'è necessità ci CHIEDERE in PRESTITO (to BORROW) informazioni dal resto dei gruppi.
### Se n_j è piccolo, ybar_j NON va bene come stima e "correggo" in modo tale che la stima bayesiana 
### si avvicini alla (stima) di mu, la media dei gruppi
x11()
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
#theta.hat= posterior means of theta_j
theta.hat<-apply(THETA,2,mean)
theta.hat
## LEFT
## ybar (le stime 'classiche', che sono sample average per scuola) 
#  on the x-axis and theta.hat (le stime bayesiane) on the y-axis
plot(ybar,theta.hat,xlab=expression(bar(italic(y))),ylab=expression(hat(theta)))
abline(0,1,col='red')
## The slope of this line is <1, that is, 
## high values of ybar.j correspond to
## slightly less high values of the Bayesian estimates of theta_j,
## and low values of ybar.j correspond to slightly less low values of 
## the Bayesian estimates of theta_j. This is the SHRINKAGE effect

## RIGHT
## group-specific sample sizes on the x-axis, and differences 
## between frequentist and Bayesian estimates on the y-axis.
plot(n,ybar-theta.hat,ylab=expression( bar(italic(y))-hat(theta) ),xlab="sample size")
abline(h=0,col='red')
## Groups with low sample size get shrunk the most, whereas groups 
## with large sample size hardly get shrunk at all. 
## The larger the sample size for a group, the more information 
## we have for that group and the less information we need 
## to BORROW from the rest of the population.
dev.off()
#

#salvataggio medie scuole
write.csv(theta.hat,file="medie_a_posteriori.csv")
#salvataggio RData
save.image(file="medievoti.RData")

#Intervalli di credibilità----------------------------------------------------------------
windows()

plot(1:474,theta.hat, cex=0.5,main="90% credible intervals",
     xlim=c(1,474),ylim=c(20,70),xlab='school',ylab='')
for (i in 1:474) {
  probint=quantile(THETA[,i], c(0.05, 0.95))
  lines(i * c(1, 1), probint)
}

