# Ora che ho capito che i modelli migliori sono anova2 e anova1, 
# per entrambi i modelli faccio dei test di ipotesi per confrontare le regioni.
# Infatti è chiaro il "podio" delle regioni, ma vorrei fare dei test per avere 
# evidenze quantitative delle conclusioni precedenti.

# L'idea è quella di proporre un test di ipotesi H0, H1 e di calcolare
# il bayes factor. La prior odd P(H0)/P(H1) è calcolato a partire da un
# campionamento dei parametri dalla loro prior e dal conteggio di 
# quante volte vale H0 sul totale. La stessa cosa viene fatta per la 
# posterior odd P(H0|Y1,...Yn)/P(H1|Y1,...Yn) ma a partire dai risultati campionati
# dal MCMC.

set.seed(1)
rm(list=ls())
setwd("~/aa POLIMI/AA_Bayesiana/Progetto Pisa/ANOVA regioni")

dati <- read.csv(file='~/aa POLIMI/AA_Bayesiana/Progetto Pisa/dati_sistemati_E_sistemazione_dati/dati_bivariato_std.csv')
names(dati)

dati <- dati[,c(1,3,16,20,25,21,28,23,24,15,31,32,22,10,9,41,42)]
dati <- na.omit(dati)
dati <- dati[which(dati$region != '0'),]
dati[,2] <- as.numeric(dati[,2])-1 # Campania=1, Lombardia=2, Trentino=3
group <- dati[,2]

# MODELLO ANOVA2                                                ###################

# H0: theta1 < theta3 < theta2            H1: otherwise         ###################
# Voglio vedere se il podio Lombardia-Trentino-Campania è vero

load("anova2_con_science_result.dat")
N <- dim(output[[1]][,])[1]

# Prior odd
# Law(theta1,theta2,theta3) = N_2(theta1; mean0, 5*I_2) x 
#                             N_2(theta2; mean0, 5*I_2) x
#                             N_2(theta3; mean0, 5*I_2)

mean0 <- c(lm(pv5math ~ escs +  gender + enjoy_science + video_games + learning_time_science +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1],
           lm(pv5read ~ escs + gender + enjoy_science + video_games + learning_time_science +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient,
              data= dati)$coefficients[1])
mean0 <- as.numeric(mean0)

I2 <- diag(rep(1,2), ncol=2)

library(mvtnorm) #per gaussiane random
theta1_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
theta2_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
theta3_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)

P0 <- sum(theta1_prior[,1] < theta3_prior[,1] & theta3_prior[,1] < theta2_prior[,1] & 
            theta1_prior[,2] < theta3_prior[,2] & theta3_prior[,2] < theta2_prior[,2])/N

PriorOdd <- P0/(1-P0)

# Posterior Odd
colnames(output[[1]][,])

theta1_post <- as.matrix(output[[1]][,4:5])
theta2_post <- as.matrix(output[[1]][,6:7])
theta3_post <- as.matrix(output[[1]][,8:9])

P0_post <- sum(theta1_post[,1] < theta3_post[,1] & theta3_post[,1] < theta2_post[,1] & 
                theta1_post[,2] < theta3_post[,2] & theta3_post[,2] < theta2_post[,2])/N

PostOdd <- P0_post/(1-P0_post)

# BF
BF_anova2 <- PostOdd/PriorOdd
BF_anova2 
2*log(BF_anova2) # 6.72 -> strong evidence in favour oh H0

# Anche se non si direbbe guardando i tre grafici, il trentino è peggiore
# della lombardia. Invece, la campania si conferma la peggiore


rm(output)


# MODELLO ANOVA1                                                          #############

# H0: beta0_1 < beta0_3< beta0_2            H1: otherwise                 #############
# Voglio vedere se il podio Lombardia-Trentino-Campania è vero

load("anova1_con_science_result.dat")
N <- dim(output[[1]][,])[1]

# Prior odd
# Law(beta01,beta02,beta03) = N_2(beta01; mean0, 5*I_2) x 
#                             N_2(beta02; mean0, 5*I_2) x
#                             N_2(beta03; mean0, 5*I_2)

mean0 <- c(lm(pv5math ~ escs +  gender + enjoy_science + video_games + learning_time_science+
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1],
           lm(pv5read ~ escs + gender + enjoy_science + video_games + learning_time_science +
                motivat + test_anxiety + cultural_possessions + study_before_school +
                study_after_school + interest_broad_science + ISCED_orient ,
              data= dati)$coefficients[1])
mean0 <- as.numeric(mean0)

I2 <- diag(rep(1,2), ncol=2)

library(mvtnorm) #per gaussiane random
beta01_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
beta02_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
beta03_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)

P0 <- sum(beta01_prior[,1] < beta03_prior[,1] & beta03_prior[,1] < beta02_prior[,1] & 
            beta01_prior[,2] < beta03_prior[,2] & beta03_prior[,2] < beta02_prior[,2])/N

PriorOdd <- P0/(1-P0)

# Posterior Odd
colnames(output[[1]][,])

beta01_post <- as.matrix(output[[1]][,1:2])
beta02_post <- as.matrix(output[[1]][,3:4])
beta03_post <- as.matrix(output[[1]][,5:6])

P0_post <- sum(beta01_post[,1] < beta03_post[,1] & beta03_post[,1] < beta02_post[,1] & 
                 beta01_post[,2] < beta03_post[,2] & beta03_post[,2] < beta02_post[,2])/N

PostOdd <- P0_post/(1-P0_post)

# BF
BF_anova1 <- PostOdd/PriorOdd
BF_anova1 
2*log(BF_anova1) # 6.7841 evidence in favour of H0

# Nemmeno in questo caso si direbbe, eppure la lombardia supera il trentino.
# La campania si conferma ultima


# MODELLO ANOVA 1 BIS                         ############################
# non sono convinta del fatto che il trentino sia peggio della lombardia
# H0: beta0_3 < beta0_2 H1: otherwise

beta02_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
beta03_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)

P0 <- sum(beta03_prior[,1] < beta02_prior[,1] & beta03_prior[,2] < beta02_prior[,2])/N

PriorOdd <- P0/(1-P0)

P0_post <- sum( beta03_post[,1] < beta02_post[,1] & beta03_post[,2] < beta02_post[,2])/N

PostOdd <- P0_post/(1-P0_post)

# BF
BF_anova1_TL <- PostOdd/PriorOdd
BF_anova1_TL
2*log(BF_anova1_TL) # 2.15 weak evidence in favour of H0


# # H0: mu_1 < mu_3< mu_2            H1: otherwise                         #############
# # Voglio vedere se il podio Lombardia-Trentino-Campania è vero confrontando
# # le medie dei voti
# 
# load("anova1_result_mu.dat")
# N <- dim(mu[[1]][,])[1]
# q <- dim(dati)[2]-4 # covariate interessanti
# 
# # Prior odd
# # mu_ij = beta0_j + Xi * beta
# # Law(beta01,beta02,beta03) = N_2(beta01; mean0, 5*I_2) x 
# #                             N_2(beta02; mean0, 5*I_2) x
# #                             N_2(beta03; mean0, 5*I_2)
# # Law(beta1) = Law(beta2) = N_q(0, 5)
# # beta = [beta1, beta2]
# 
# mean0 <- c(lm(pv5math ~ escs +  gender + enjoy_science + video_games + learning_time_math +
#                 motivat + test_anxiety + cultural_possessions + study_before_school +
#                 study_after_school + interest_broad_science + ISCED_orient + student_teacher_ratio,
#               data= dati)$coefficients[1],
#            lm(pv5read ~ escs + gender + enjoy_science + video_games + learning_time_math +
#                 motivat + test_anxiety + cultural_possessions + study_before_school +
#                 study_after_school + interest_broad_science + ISCED_orient + student_teacher_ratio,
#               data= dati)$coefficients[1])
# mean0 <- as.numeric(mean0)
# 
# I2 <- diag(rep(1,2), ncol=2)
# Iq <- diag(rep(1,q), ncol=q)
# 
# library(mvtnorm) #per gaussiane random
# beta01_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
# beta02_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
# beta03_prior <- rmvnorm(N, mean= mean0, sigma= 5*I2)
# 
# beta1_prior <- rmvnorm(N, mean=rep(0,q), sigma=5*Iq); dim(beta1_prior)
# beta2_prior <- rmvnorm(N, mean=rep(0,q), sigma=5*Iq)
# 
# X1 <- colMeans(dati[which(dati[,2]==1), 3:(q+2)])
# X2 <- colMeans(dati[which(dati[,2]==2), 3:(q+2)])
# X3 <- colMeans(dati[which(dati[,2]==3), 3:(q+2)])
# 
# mu1_prior <- beta01_prior + cbind(beta1_prior%*%as.matrix(X1),beta2_prior%*%as.matrix(X1))
# mu2_prior <- beta02_prior + cbind(beta1_prior%*%as.matrix(X2),beta2_prior%*%as.matrix(X2))
# mu3_prior <- beta03_prior + cbind(beta1_prior%*%as.matrix(X3),beta2_prior%*%as.matrix(X3))
# 
# P0 <- sum(mu1_prior[,1] < mu3_prior[,1] & mu3_prior[,1] < mu2_prior[,1] & 
#             mu1_prior[,2] < mu3_prior[,2] & mu3_prior[,2] < mu2_prior[,2])/N
# 
# PriorOdd <- P0/(1-P0)
# 
# # Posterior Odd
# load("anova1_result_mu.dat")
# dim(mu[[1]][,])[2]/2# 4468
# mu_math <- as.matrix(mu[[1]][,1:4468])
# mu_math <- cbind(
#   rowMeans(mu_math[,which(group==1)]),
#   rowMeans(mu_math[,which(group==2)]),
#   rowMeans(mu_math[,which(group==3)])  )
# 
# mu_read <- as.matrix(mu[[1]][,4469:8936])
# mu_math <- cbind(
#   rowMeans(mu_read[,which(group==1)]),
#   rowMeans(mu_read[,which(group==2)]),
#   rowMeans(mu_read[,which(group==3)])  )
# 
# mu1_post <- cbind(mu_math[,1],mu_read[,1])
# mu2_post <- cbind(mu_math[,2],mu_read[,2])
# mu3_post <- cbind(mu_math[,3],mu_read[,3])
# 
# P0_post <- sum(mu1_post[,1] < mu3_post[,1] & mu3_post[,1] < mu2_post[,1] & 
#                  mu1_post[,2] < mu3_post[,2] & mu3_post[,2] < mu2_post[,2])/N
# 
# PostOdd <- P0_post/(1-P0_post) # 0, perchè non avviene mai che mu3_[,2]<mu2[,2]
# # Perciò, il podio non è confermato
# 
# # H0: mu_1 < mu_3, mu_1 < mu_2            H1: otherwise                         #############
# # Voglio vedere se la Campania è sempre la peggiore
# 
# 
# P0 <- sum(mu1_prior[,1] < mu3_prior[,1] & mu1_prior[,1] < mu2_prior[,1] & 
#             mu1_prior[,2] < mu3_prior[,2] & mu1_prior[,2] < mu2_prior[,2])/N
# 
# PriorOdd <- P0/(1-P0)
# 
# P0_post <- sum(mu1_post[,1] < mu3_post[,1] & mu1_post[,1] < mu2_post[,1] & 
#                  mu1_post[,2] < mu3_post[,2] & mu1_post[,2] < mu2_post[,2])/N
# 
# PostOdd <- P0_post/(1-P0_post) # P0_post = 1
# 
# # perciò abbiamo enorme evidenza a favore di H0


