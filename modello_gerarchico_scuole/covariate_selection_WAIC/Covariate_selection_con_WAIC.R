# day 7.2

# 1) prima lancio il modello mixed effect con solo le cov studente
# 2) alle cov studente, aggiungo una per volta le cov scuola più significative 
#    e lancio dei modelli diversi
# 3) per scegliere quale cov scuola è migliore, controllo tutto con l'indice waic

library(rjags)
library(coda)


# 1) LMM con covariate studente, raggruppati per scuola --------------------------------

# In questo caso, consideriamo il modello seguente:
# i = studente nella j = scuola
# Y_ij =  theta0 + theta_j + gamma_ij * X_ij + err_ij

inits1 <- function(p, ngr){
    list(
    theta0 = 0.1,
    theta = rep(0.1, ngr),
    tau2_theta = rep(1/50, ngr),
    gamma = matrix(0.1, p, ngr),
    tau2_gamma = matrix(1/50, p, ngr),
    tau2 = 1/100
    )
}

setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/risultati_WAIC2")
dati <- read.csv("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/dati_hierarchical/data_hierarchical_WAIC.csv")

names(dati)
dati <- dati[,-c(1:2, 24:30)] # !!!!! TOLGO ANCHE COSE SCUOLA

ngr <- length(unique(dati$school_id)) #numero di scuole
N <- dim(dati)[1] # numero studenti
p <- dim(dati)[2]-1-1 # tolgo gruppo e voto

model <- jags.model("simple_LMM2.bug",
                    data = list(Y = dati[,2],
                                X = dati[,3:dim(dati)[2]], # effetto random
                                gr = dati[,1],
                                p = p,
                                N = N,
                                ngr = ngr),
                    n.adapt = 1000,
                    inits = inits1(p, ngr),
                    n.chains = 1) 

update(model,n.iter=14000)

output <- coda.samples(model = model,
                       variable.names = c("gamma", "theta", 
                                          "tau2", "lik",
                                          'theta0', 'tau2_theta0',
                                          'tau2_theta', 'tau2_gamma'),
                       n.iter = 50000,
                       thin = 10)

save(output, file = 'output solo studenti.dat')

#.. analisi grafica di convergenza -------------------------------------
# 
# load('output solo studenti.dat')
# 
# colonne <- which(colnames(output[[1]])=='theta[1]') : which(colnames(output[[1]])=='theta[297]')
# u <- which(runif(ngr, min=0, max=1) <=0.2) + colonne[1]
# output_theta <- output[[1]][,u]
# output_theta <- mcmc.list(output_theta=output_theta)
# 
# colonne2 <- which(colnames(output[[1]])=='gamma[1,1]') : (which(colnames(output[[1]])=='tau2')-1)
# u <- which(runif(length(colonne2), min=0, max=1) <=0.01) 
# output_gamma <- output[[1]][,u]
# output_gamma <- mcmc.list(output_gamma=output_gamma)
# 
# x11(); plot(output_theta)
# 
# geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z
# pnorm(abs(geweke.diag(output_theta, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2 
# 
# x11();acfplot(output_theta, lag.max = 50)
# 
# 
# x11(); plot(output_gamma)
# 
# geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z
# pval <- pnorm(abs(geweke.diag(output_gamma, frac1=0.1, frac2=0.5)[[1]]$z),lower.tail=FALSE)*2
# sum(pval <=0.05)/length(pval)
# 
# x11();acfplot(output_gamma, lag.max = 50)
# 
# graphics.off()
# 

# 2) LMM covariate studente e una covariata scuola per volta -------------------
rm(list=ls())

# In questo caso, consideriamo i modelli seguenti:
# i = studente nella j = scuola
# Y_ij =  theta0 + gamma0_j + theta * Z_i + gamma_ij * X_ij + err_ij
# X contiene i dati sullo studente
# Z contiene i dati sulle scuole

# funzioni che utilizzerò per il modello:
# funzione che inizializza i parametri di modello
inits2 <- function(ngr,p){
  list(
    theta0 = 0.1,
    theta = 0.1,
    gamma = matrix(0.1, p, ngr),
    gamma0 = rep(0.1, ngr),
    tau2 = 1/100
  )
}

#funzione che calcola il modello
my_algorithm_LMM2 <- function(dati){
  
  #nell'input DATI deve esserci  il gruppo come prima colonna, 
  #la risposta come seconda colonna,
  #le p cov studente dopo,
  #e infine la cov scuola
  
  N <- dim(dati)[1]
  p <- dim(dati)[2]-1-1-1 # tolgo gruppo, voto, cov.scuola
  
  gr <- dati[,1]
  ngr <- length(unique(gr))
  Y <- dati[,2]
  dati_studente <- dati[,3: (dim(dati)[2]-1) ]
  cov_scuola <- dati[,dim(dati)[2]]
  
  model <- jags.model(file='one_school_LMM2.bug',
                     data= list(Y = Y,
                                X = dati_studente,
                                Z = cov_scuola,
                                N = N, 
                                p = p,
                                ngr = ngr,
                                gr = gr),
                     n.adapt = 1000,
                     inits = inits2(ngr,p),
                     n.chains = 1) 

  update(model, n.iter=14000)
  
  output <- coda.samples(model = model,
                         variable.names = c("gamma", "theta", 
                                            "tau2", "lik", 'gamma0',
                                            'theta0', 'tau2_theta0',
                                            'tau2_theta', 'tau2_gamma',
                                            'tau2_gamma0'),
                         n.iter = 50000,
                         thin = 10)
  return(output)

}

#inizio a lavorare davvero
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/risultati_WAIC2")

dati <- read.csv("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/LMM-hierarchical/dati_hierarchical/data_hierarchical_WAIC.csv")
dati <- dati[,-c(1:2)]
names(dati)
# Abbiamo scelto i modelli con, come covariata scuola, private_independent
# (una dummy), school_size, ISCED_orient, certified teacher prop, sci teacher
# prop, student hindering behaviour, student teaher ratio

col_scuola <- 22:28 # colonna in dati della covariate scuola da controllare

for (i in 1:length(col_scuola)){
  
  print(colnames(dati)[col_scuola[i]])
  output <- my_algorithm_LMM2( dati[,c(1:21, col_scuola[i])] )
  save(output, file= paste(colnames(dati)[col_scuola[i]],'WAIC output.dat'))
  rm(output)
  
}



# 3) Confronto i modelli con il WAIC -------------------------------
# NB: utilizza il workspace della sezione 2) LMM, anche senza output ma che almeno ci siano i dati

# funzione che, dato un output del jags.model, calcola l'inidice WAIC del modello
# NB: è da dare anche il vettore delle colonne di output con il valore della likelihood
my_WAIC <- function(output, colonne_lik){
  
  lppd <- sum( apply(output[[1]][, colonne_lik], 2, function(x) log(mean(x)) ) )
  p_waic <- sum( apply(output[[1]][, colonne_lik],2, function(x) var(log(x)) )  )
  
  WAIC   <- lppd - p_waic #dalle slide prof ; -2*lppd + 2*p_WAIC da corradin
  return(WAIC)
}

# carico i risultati del modello e calcolo il WAIC del modello
col_scuola <- 22:28 # colonna in dati della covariate scuola da controllare
WAIC <- numeric(1+length(col_scuola))

load("output solo studenti.dat")
col <- which(colnames(output[[1]])=='lik[1]') : (which( colnames(output[[1]])=='tau2')-1 );
WAIC[1] <- my_WAIC(output, col) 
rm(output)

for(i in 1:length(col_scuola)){
  load(paste(colnames(dati)[col_scuola[i]],'WAIC output.dat'))
  colonne <- which(colnames(output[[1]])=='lik[1]'):(which(colnames(output[[1]])=='tau2')-1) 
  WAIC[i+1] <- my_WAIC(output, colonne)
  rm(output)
  print(i)
}

names(WAIC) <- c('solo_stud', colnames(dati)[col_scuola])
WAIC
save(WAIC, file="risultati_WAIC.dat")

WAIC-WAIC[1]
# Conclusioni ------------------------------------------
# Vagamente interessanti risultano le covariate ISCED_orient e certified_teacher_prop
# Tuttavia, il WAIC varia di solo qualche punto (7.55, 6.88)