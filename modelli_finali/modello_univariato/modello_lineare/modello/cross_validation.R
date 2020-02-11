
rm(list = ls())

#cross validation tra modello sei e sette
load('LM1.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate_uno=(quantili[1:6,3])
inter=quantili[7,3]
err_uno=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate_uno
  
  err_uno[i]=abs(mate-prev_mate)
}



#cross validation tra modello sei e sette
load('LM2.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:9,3])
inter=quantili[10,3]
err_due=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_due[i]=abs(mate-prev_mate)
}






#cross validation tra modello sei e sette
load('LM3.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:11,3])
inter=quantili[12,3]
err_tre=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_tre[i]=abs(mate-prev_mate)
}












#cross validation tra modello sei e sette
load('LM4.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:13,3])
inter=quantili[14,3]
err_quattro=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_quattro[i]=abs(mate-prev_mate)
}




#cross validation tra modello sei e sette
load('LM5.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:15,3])
inter=quantili[16,3]
err_cinque=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_cinque[i]=abs(mate-prev_mate)
}






#cross validation tra modello sei e sette
load('LM6.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:17,3])
inter=quantili[18,3]
err_sei=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_sei[i]=abs(mate-prev_mate)
}






#cross validation tra modello sei e sette
load('LM7.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:19,3])
inter=quantili[20,3]
err_sette=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_sette[i]=abs(mate-prev_mate)
}







errori=c(mean(err_uno),mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei),mean(err_sette))

plot(errori, col = "black", lwd = 5)








#cross validation tra modello sei e sette
load('LM4_no_mate.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:12,3])
inter=quantili[13,3]
err_quattro_no_mate=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_quattro_no_mate[i]=abs(mate-prev_mate)
}


#cross validation tra modello sei e sette
load('LM4_no_scie.rdata')
quantili=as.matrix(summary(output)$quantiles)
beta_mate=(quantili[1:12,3])
inter=quantili[13,3]
err_quattro_no_scie=rep(0,N)
for (i in 1:N){
  
  mate=Y[i]
  
  prev_mate=inter+X[i,]%*%beta_mate
  
  err_quattro_no_scie[i]=abs(mate-prev_mate)
}




errori=c(mean(err_uno),mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei),mean(err_sette),mean(err_quattro_no_mate),mean(err_quattro_no_scie))

plot(errori, col = "black", lwd = 5)



