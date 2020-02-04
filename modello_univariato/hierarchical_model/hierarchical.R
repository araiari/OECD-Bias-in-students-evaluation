#controllo numerosità scuole
rm(list = ls())
library(MASS)
set.seed(49)
dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]
Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
dim_scuola=rep(0,M)
for (i in 1:N){
  dim_scuola[g[i]]=dim_scuola[g[i]]+1
}




























rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,3]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


ZZ=as.matrix(dati[,c(5:23)])
ZZ=ZZ[,c(3,4,5,10,11,12)]#seleziono solo 6 covaraite studente
Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

for (i in 1:dim(ZZ)[1]){
  for(j in 1:dim(ZZ)[2]){
    Z[i,j]=as.numeric(ZZ[i,j])
  }
}

p = dim(X)[2]
v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    Z = as.matrix(Z))

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif.bug",
                 data = data_JAGS_EN,
                 n.adapt = 40000,
                 inits = inits,
                 n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
param <- c('theta0', "theta",'gamma0','gamma')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod1_nonstand_congamma.Rdata')


x11()
plot(output[[1]][,3860:4158],ask=T)
#dev.off()






rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,c(3,7)]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


ZZ=as.matrix(dati[,c(5:23)])
ZZ=ZZ[,c(3,4,5,10,11,12)]#seleziono solo 6 covaraite studente
Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

for (i in 1:dim(ZZ)[1]){
  for(j in 1:dim(ZZ)[2]){
    Z[i,j]=as.numeric(ZZ[i,j])
  }
}

p = dim(X)[2]
v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    Z = as.matrix(Z))

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif.bug",
                   data = data_JAGS_EN,
                   n.adapt = 40000,
                   inits = inits,
                   n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
param <- c('theta0', "theta",'gamma0','gamma')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod3_nonstand_congamma.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()
















rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,3]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


#ZZ=as.matrix(dati[,c(5:23)])
#ZZ=ZZ[,c(3,4,5,10,11,12)]#seleziono solo 6 covaraite studente
#Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

#for (i in 1:dim(ZZ)[1]){
#  for(j in 1:dim(ZZ)[2]){
#    Z[i,j]=as.numeric(ZZ[i,j])
#  }
#}

p = dim(X)[2]
#v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    #                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X)
                    #                    Z = as.matrix(Z)
)

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    #gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    #    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif_nostud.bug",
                   data = data_JAGS_EN,
                   n.adapt = 60000,
                   inits = inits,
                   n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'tau2' )
param <- c("theta0", "theta", "gamma0")
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod2_nonstand_congamma.Rdata')


x11()
plot(output,ask=T)
#dev.off()






rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,c(3,7)]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


#ZZ=as.matrix(dati[,c(5:23)])
#ZZ=ZZ[,c(3,4,5,10,11,12)]#seleziono solo 6 covaraite studente
#Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

#for (i in 1:dim(ZZ)[1]){
#  for(j in 1:dim(ZZ)[2]){
#    Z[i,j]=as.numeric(ZZ[i,j])
#  }
#}

p = dim(X)[2]
#v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    #                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X)
                    #                    Z = as.matrix(Z)
)

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    #    gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    #    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif_nostud.bug",
                   data = data_JAGS_EN,
                   n.adapt = 60000,
                   inits = inits,
                   n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
param <- c("theta0", "theta", "gamma0")
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod4_nonstand_congamma.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()
















rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,3]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


ZZ=as.matrix(dati[,c(5:23)])
ZZ=ZZ[,c(2,3,4,5,7,8,9,10,11,12,15,18,19)]#seleziono 13 covaraite studente
Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

for (i in 1:dim(ZZ)[1]){
  for(j in 1:dim(ZZ)[2]){
    Z[i,j]=as.numeric(ZZ[i,j])
  }
}

p = dim(X)[2]
v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    Z = as.matrix(Z))

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif.bug",
                   data = data_JAGS_EN,
                   n.adapt = 40000,
                   inits = inits,
                   n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
param <- c('theta0', "theta",'gamma0','gamma')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod5_nonstand_congamma.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()






rm(list = ls())
library(rjags)
library(coda)

# for plots
library(ggplot2)
library(MASS)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(plot.matrix)
require(gplots)
require(ggpubr)

set.seed(49)


dati=read.csv('data_hierarchical_WAIC.csv', header=T)
N = dim(dati)[1]

Y=as.vector(dati[,4]) #controllare quale colonna è la risposta
#Y=(Y-mean(Y))/sd(Y)
g = as.factor(dati[,3])
M=length(levels(g))
levels(g)=c(1:M)
g=as.numeric(g)
XX=as.matrix(dati[,c(24:30)])#covariate scuola
XX[,2]=(XX[,2]-mean(XX[,2]))/sd(XX[,2])
XX[,4]=(XX[,4]-mean(XX[,4]))/sd(XX[,4])
XX[,5]=(XX[,5]-mean(XX[,5]))/sd(XX[,5])
XX[,6]=(XX[,6]-mean(XX[,6]))/sd(XX[,6])
XX[,7]=(XX[,7]-mean(XX[,7]))/sd(XX[,7])
XX=as.matrix(XX[,c(3,7)]) #3ISCED, 7student_teacher_ratio
X=matrix(0,dim(XX)[1], dim(XX)[2])

for (i in 1:dim(XX)[1]){
  for(j in 1:dim(XX)[2]){
    X[i,j]=as.numeric(XX[i,j])
  }
}


ZZ=as.matrix(dati[,c(5:23)])
ZZ=ZZ[,c(2,3,4,5,7,8,9,10,11,12,15,18,19)]#seleziono 13 covaraite studente
Z=matrix(0,dim(ZZ)[1], dim(ZZ)[2])

for (i in 1:dim(ZZ)[1]){
  for(j in 1:dim(ZZ)[2]){
    Z[i,j]=as.numeric(ZZ[i,j])
  }
}

p = dim(X)[2]
v = dim(Z)[2]




data_JAGS_EN <-list(N = N, 
                    p = p, 
                    v = v,
                    M = M, 
                    Y = Y,
                    g = as.vector(g),
                    X = as.matrix(X),
                    Z = as.matrix(Z))

inits = function() {
  list(#theta0 = 0.0, 
    theta = rep(0,p),
    gamma0 = rep(0,M),
    gamma = matrix(0,M,v),
    
    tau2  = 5,
    tau2_theta0=0.1,
    tau2_gamma0=rep(5, M),
    tau2_theta=rep(5, p),
    prec_gamma=rep(5, v),
    tau = rep(5, p), #a normal exp(0.1), a big exp(0.01), a small exp(10)
    .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

model = jags.model("hierarchical_unif.bug",
                   data = data_JAGS_EN,
                   n.adapt = 40000,
                   inits = inits,
                   n.chains = 1) 

nit <- 20000
thin <-10

#param <- c("theta0", "theta", "gamma",'a1','a2')
#param <- c("theta0", "theta",'a1','a2')
#param <- c("theta0", "theta",'a1','a2','a')
#param <- c("theta0", "theta",'a1','a2','prec_gamma')
#param <- c( "theta",'a1','a2','prec_gamma')
param <- c('theta0', "theta",'gamma0','gamma')
#param <- c( "theta",'a1','a2','prec_gamma','gamma')
output <- coda.samples(model = model,
                       variable.names = param,
                       n.iter = nit,
                       thin = thin)
save.image(file='mod6_nonstand_congamma.Rdata')


#x11()
#plot(output,ask=T)
#dev.off()

