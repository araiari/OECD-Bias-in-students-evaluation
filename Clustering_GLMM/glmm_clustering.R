##############################################################################
# Modelli lineari generalizzati con prior nonparametrica sugli effetti casuali
#############  GLMM con JAGS   su studenti e scuole (PISA)   #################
##############################################################################
rm(list=ls())

# DATI CENTRATI
# Importo i dati e li sistemo
input = read.csv("dati_all.csv",header=T)

names(input) 
input <- input[,c(44,45,35,48,41,43,9,2)]
names(input) 

summary(input)
input <- na.omit(input)
input[,8] <- as.factor(as.character(input[,8]))
length(levels(input[,8]))
levels(input[,8]) <- as.character(1:462)
head(input)
input[2200:2230,] #ok
input[,8] <- as.numeric(input[,8])
#normalizzo risposta
input[,1] <- (input[,1]-mean(input[,1]))/sd(input[,1])
summary(input)

#salvataggio dei dati (facoltativo)
write.csv(input, file="dati_glmmDP.csv")

n.data=dim(input)[1] # numero totale studenti
J=462 # numero scuole
M=40 # troncamento serie Sethuraman (nel file BUG poi avviene la normalizzazione)
## le variabili sono standardizzate

#######################################
library(rjags)      # interfaccia R con JAGS
library(plotrix)    # per fare plot CIs
set.seed(1)         # fisso il seed per poter riprodurre


#  genero una lista con i dati da passare a JAGS
data <- list(Y=input$pv5math,
             GENDER=input$gender, ESCS=input$escs, VGAMES=input$video_games,
             INTSCI=input$interest_broad_science, ANXIETY=input$test_anxiety,
             ISCEDO=input$ISCED_orient, SCHOOL=input$school_id, n=n.data,J=J,M=M)

###########  
# definisco una lista con lo stato iniziale della catena
# fornisco anche il seed e il tipo di generatore di numeri casuali
# theta sono i location points della mistura (serie di Sethuraman troncata)

r=rep(0.5,M)
theta=rep(0,M)
S=rep(1,J)

inits = list(beta=rep(0,6),a = 1,  
             lambda.bb = 1,
             r=r,
             theta= theta,
             S = S,
             Snew = 1,
             .RNG.seed = 2,
             .RNG.name = 'base::Wichmann-Hill'
)

#########################
### CREATE JAGS MODEL ###
#########################

modelGLMM_DP=jags.model("GLMM_DP_Pisa6.bug",data=data,inits=inits,n.adapt=1000,n.chains=1) 
# Faccio un update del modello per il numero di iterazioni specificato SENZA salvare nulla 
update(modelGLMM_DP,20000)

variable.names=c("bb", "beta",  "tau.bb", "newschool", "alpha","K")
# Monitoro i parametri: 
n.iter=50000
thin=20  

outGLMM_DP=coda.samples(model=modelGLMM_DP,variable.names=variable.names,n.iter=n.iter,thin=thin)
# salvo l'intera catena dei parametri monitorati (si tratta di una lista mcmc)

save(outGLMM_DP,file='GLMMDP_Pisa_output.Rdata')

##############
### OUTPUT ###
##############
rm(list=ls())

# Importo i dati 
input = read.csv("dati_glmmDP.csv",header=T)
input <- input[,-1]
names(input) 

n.data=dim(input)[1] # numero totale studenti
J=462 # numero scuole
M=40 # troncamento serie Sethuraman


library(coda)        # pacchetto per analizzare catene
library(plotrix)     # per fare plot CIs

load('GLMMDP_Pisa_output6.Rdata') # carico l'output ottenuto con coda.samples

#  summary(output)

data=as.matrix(outGLMM_DP) # trasformo il dataframe in matrice 
data=data.frame(data)
attach(data)
n.chain=dim(data)[1]   # lunghezza catena (final sample size)

##################
### TRACEPLOTS ###
##################

par(mfrow=c(2,3))
plot(data[,'beta.1.'],main='gender',type='l')
plot(data[,'beta.2.'],main='escs',type='l')
plot(data[,'beta.3.'],main='video_games',type='l')
plot(data[,'beta.4.'],main='interest_broad_science',type='l')
plot(data[,'beta.5.'],main='test_anxiety',type='l')
plot(data[,'beta.6.'],main='ISCED_orient',type='l')

par(mfrow=c(2,3))
plot(density(data[,'beta.1.']),main='gender')
plot(density(data[,'beta.2.']),main='escs')
plot(density(data[,'beta.3.']),main='video_games')
plot(density(data[,'beta.4.']),main='interest_broad_science')
plot(density(data[,'beta.5.']),main='test_anxiety')
plot(density(data[,'beta.6.']),main='ISCED_orient')


mean(data[,'beta.1.']<0)
mean(data[,'beta.2.']<0)
mean(data[,'beta.3.']>0)
mean(data[,'beta.4.']<0)
mean(data[,'beta.5.']<0)
mean(data[,'beta.6.']<0)

#PLOT ALTERNATIVO - TUTTO INSIEME
x11()
par(mfrow=c(3,4))
plot(data[,'beta.1.'],main='Trace of gender',type='l')
plot(density(data[,'beta.1.']),col='forest green',lwd=2,main='Density of gender')

plot(data[,'beta.2.'],main='Trace of escs',type='l')
plot(density(data[,'beta.2.']),col='forest green',lwd=2,main='Density of escs')

plot(data[,'beta.3.'],main='Trace of video_games',type='l')
plot(density(data[,'beta.3.']),col='red',lwd=2,main='Density of video_games')
abline(v=0,col='black',lwd=2)

plot(data[,'beta.4.'],main='Trace of interest_broad_science',type='l')
plot(density(data[,'beta.4.']),col='forest green',lwd=2,main='Density of interest_broad_science')

plot(data[,'beta.5.'],main='Trace of test_anxiety',type='l')
plot(density(data[,'beta.5.']),col='red',lwd=2,main='Density of test_anxiety')

plot(data[,'beta.6.'],main='Trace of ISCED_orient',type='l')
plot(density(data[,'beta.6.']),col='red',lwd=2,main='Density of ISCED_orient')



x11()
par(mfrow=c(3,3)) #qualche traceplot degli effetti casuali (ce ne sono tanti quanti sono le scuole)
plot(data[,'bb.111.'],main='b111',type='l')
plot(data[,'bb.112.'],main='b112',type='l')
plot(data[,'bb.331.'],main='b331',type='l')
plot(data[,'bb.271.'],main='b271',type='l')
plot(data[,'bb.201.'],main='b201',type='l')
plot(data[,'bb.231.'],main='b231',type='l')
plot(data[,'bb.351.'],main='b351',type='l')
plot(data[,'bb.61.'],main='b61',type='l')
plot(data[,'bb.417.'],main='b417',type='l') 

x11()
par(mfrow=c(1,2))
plot(data[,'alpha'],main='massa totale',type='l') #se ho assegnato una prior per alpha
plot(data[,'K'],main='K_n',type='l')

x11()
par(mfrow=c(1,2))
plot(density(data[,'alpha']),main='massa totale')
plot(table(data[,'K']),main='K_n')
mean(alpha);var(alpha)
mean(data[,'K']);var(data[,'K'])

par(mfrow=c(1,3))
plot(data[,'tau.bb'],main='tau',type='l')
plot(1/sqrt(data[,'tau.bb']),main='lambda',type='l')
plot(density(1/sqrt(data[,'tau.bb'])),main='lambda')


######################################
##  Plot IC per intercetta casuale  ##  
######################################

### ordered by number of students ###
# creo un vettore contenente il numero di studenti per ogni scuola
students=rep(0,J)
for(i in 1:J){
  students[i]=length(which(input$school_id==i))
}
sort_students=sort(students,index.return=T)   # ordino


# ricalcolo i quantili per ogni scuola in ordine di numero di studenti
Q=matrix(nrow=J+1, ncol=3) 
for (j in 1:J){
  Q[j,]=quantile(data[, 2 + sort_students$ix[j]  ],probs=c(0.025,0.5,0.975))
}
Q[J+1,]=quantile(data$newschool,probs=c(0.025,0.5,0.975))
colnames(Q) <- c("2.5","median","97.5")


# symbol for point estimate: all points are round, 
#    the last point is x (new random school)
pch=c(rep(21,J),4)   
x11()
plotCI(x=seq(1,J+1),y=Q[,2],uiw=(Q[,3]-Q[,2]) ,liw=(Q[,2]-Q[,1]),pch=pch,
       scol=c(rep('black',J),'magenta') , xlab="schools (sort by increasing number of students)", 
       ylab="b_j", main="CIs of the Random Intercept",col=c(rep('black',J),'magenta'),
       lwd=c(rep(1,J),2))  
abline(h=mean(Q[,2]))

##################################
# Cluster estimate di Lau&Green
################################
data=data.frame(data)  # convert into a dataframe

label.mat = as.matrix(data[,3:(3+J-1)]) # extract cluster labels

m=J 
G=n.chain
pihat <- matrix(0,ncol=m,nrow=m)
for(i in 1:G){
  ss <- label.mat[i,]
  cij <- outer(ss,ss,'==')
  pihat <- pihat+cij
}

pihat <- pihat/G

#####Binder loss function
FF <- vector("numeric")
K <- 0.18 # (K più alto dà più cluster)
#K = b/(a+b), dove 'b' è il costo di assegnare erroneament 2 cluster uguali, 'a' quello di assegnare
#erroneamente due cluster diversi.
for(i in 1:G){
  ss <- label.mat[i,] 
  cij <- outer(ss,ss,'==')
  pluto <- (pihat-K)*as.matrix(cij)
  pluto <-  pluto[upper.tri(pluto)]
  FF[i] <- sum(pluto)
}

x11()
plot(FF)

ind.bind <- which.max(FF)[1]
label.mat[ind.bind,]#leti(ind.bind,L,G,m)
#plot(FF)
ll.bind <- label.mat[ind.bind,] #leti(ind.bind,L,G,m)
unici <- unique(ll.bind)
unici
l.uni <- length(unici)# numero di gruppi stimato 
l.uni

ncl=l.uni
for(i in 1:ncl){
  print(as.numeric(which(ll.bind==unici[i])))
}

##########################################################################à
### Disegno le intercette casuali al primo livello con colori diversi a seconda del gruppo
Sest=rep(0,J)
for(i in 1:ncl){
  Sest[as.numeric(which(ll.bind==unici[i]))] = i
}

Sest
table(Sest)

#salvataggio label
write.csv(Sest, file='labels.csv')

nclusLG=length(unique(Sest))
nclusLG

bins = as.numeric(names(table(Sest)))
freqs = as.vector(table(Sest))


label=rep(0,J)

mylist=list()

for(i in 1:nclusLG)
{
  in_gr=which(Sest==bins[i])
  label[in_gr]=i
  mylist[[i]]=in_gr
}
label
mylist

###
### Ogni colore è un gruppo

gr1 <- mylist[[1]]
gr2 <- mylist[[2]]
gr3 <- mylist[[3]]
gr4 <- mylist[[4]]
gr5 <- mylist[[5]]
gr6 <- mylist[[6]]
gr7 <- mylist[[7]]


colore = rep('ciao',J)
colore[gr1]='blue'   
colore[gr2]='cyan'    
colore[gr3]='green'   
colore[gr4]='red'
colore[gr5]='magenta'
colore[gr6]='yellow'
colore[gr7]='orange'


pch=rep(20,J)
# Nella matrice 'data' dalla colonna 3 in poi ci sono i b,
# gli effetti casuali (school-specific) al primo livello
# Per verificare il contenuto delle colonne di 'data': names(data) 
# ricalcolo i quantili per ogni scuola in ordine di numero di studenti
Q=matrix(nrow=J, ncol=3) #matrix(nrow=J, ncol=3) 
for (j in 1:J){
  Q[j,]=quantile(data[, 2 + j ],probs=c(0.025,0.5,0.975))
}

x11()
plotCI(x=seq(1,J),y=Q[,2],uiw=(Q[,3]-Q[,2]) ,liw=(Q[,2]-Q[,1]),pch=pch,xaxt='n',
       col=colore,scol=colore,xlab="school",ylab=' ', main=" ", lwd=2)  
axis(side=1,at=seq(1,J),labels=seq(1,J),cex.axis=0.8)

#media di tutte le mediane
media <- mean(Q[,2])
abline(h= media)
