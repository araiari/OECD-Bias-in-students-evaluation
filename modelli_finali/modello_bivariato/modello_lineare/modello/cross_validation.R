#CROSS VALIDATION PER CONFRONTARE I MODELLI LINEARI BIVARIATI COSTRUITI



rm(list = ls())

load('risultato_modello_biv_sei.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_sei=(quantili[1:24,3])
beta_mate_sei=coeff_sei[3:13]
beta_read_sei=coeff_sei[14:24]
err_sei=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_sei[1]+X[i,]%*%beta_mate_sei
  prev_read=coeff_sei[2]+X[i,]%*%beta_read_sei
  err_sei[i]=abs(mate-prev_mate)+abs(read-prev_read)
}






load('risultato_modello_biv_sette.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_sette=(quantili[1:28,3])
beta_mate_sette=coeff_sette[3:15]
beta_read_sette=coeff_sette[16:28]
err_sette=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_sette[1]+X[i,]%*%beta_mate_sette
  prev_read=coeff_sette[2]+X[i,]%*%beta_read_sette
  err_sette[i]=abs(mate-prev_mate)+abs(read-prev_read)
}












load('risultato_modello_biv_nove.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_nove=(quantili[1:36,3])
beta_mate_nove=coeff_nove[3:19]
beta_read_nove=coeff_nove[20:36]
err_nove=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_nove[1]+X[i,]%*%beta_mate_nove
  prev_read=coeff_nove[2]+X[i,]%*%beta_read_nove
  err_nove[i]=abs(mate-prev_mate)+abs(read-prev_read)
}





load('risultato_modello_biv_otto.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_otto=(quantili[1:32,3])
beta_mate_otto=coeff_otto[3:17]
beta_read_otto=coeff_otto[18:32]
err_otto=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_otto[1]+X[i,]%*%beta_mate_otto
  prev_read=coeff_otto[2]+X[i,]%*%beta_read_otto
  err_otto[i]=abs(mate-prev_mate)+abs(read-prev_read)
}








load('risultato_modello_biv_due.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_due=(quantili[1:8,3])
beta_mate_due=coeff_due[3:5]
beta_read_due=coeff_due[6:8]
err_due=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_due[1]+X[i,]%*%beta_mate_due
  prev_read=coeff_due[2]+X[i,]%*%beta_read_due
  err_due[i]=abs(mate-prev_mate)+abs(read-prev_read)
}





load('risultato_modello_biv_tre.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_tre=(quantili[1:12,3])
beta_mate_tre=coeff_tre[3:7]
beta_read_tre=coeff_tre[8:12]
err_tre=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_tre[1]+X[i,]%*%beta_mate_tre
  prev_read=coeff_tre[2]+X[i,]%*%beta_read_tre
  err_tre[i]=abs(mate-prev_mate)+abs(read-prev_read)
}





load('risultato_modello_biv_quattro.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_quattro=(quantili[1:16,3])
beta_mate_quattro=coeff_quattro[3:9]
beta_read_quattro=coeff_quattro[10:16]
err_quattro=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_quattro[1]+X[i,]%*%beta_mate_quattro
  prev_read=coeff_quattro[2]+X[i,]%*%beta_read_quattro
  err_quattro[i]=abs(mate-prev_mate)+abs(read-prev_read)
}




load('risultato_modello_biv_cinque.rdata')
quantili=as.matrix(summary(output)$quantiles)
coeff_cinque=(quantili[1:20,3])
beta_mate_cinque=coeff_cinque[3:11]
beta_read_cinque=coeff_cinque[12:20]
err_cinque=rep(0,N)
for (i in 1:N){
  
  mate=Y[i,1]
  read=Y[i,2]
  prev_mate=coeff_cinque[1]+X[i,]%*%beta_mate_cinque
  prev_read=coeff_cinque[2]+X[i,]%*%beta_read_cinque
  err_cinque[i]=abs(mate-prev_mate)+abs(read-prev_read)
}

errori=c(mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei),mean(err_sette),mean(err_otto),mean(err_nove))

plot(errori,   col = "black", lwd = 5)