#CROSS VALIDATION BIVARIATA
#NON SI CONSIDERA IL MODELLO UNO (ERA UNA PROVA). I MODELLI SONO DAL DUE AL NOVE






rm(list = ls())
#cross validation tra modello sei e sette
load('risultato_modello_biv_sei.rdata')
out=output[[1]]
coeff_sei=out[,1:24]
beta_mate_sei=out[,3:13]
beta_read_sei=out[,14:24]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_sei=mean(err)





#cross validation tra modello sei e sette
load('risultato_modello_biv_sette.rdata')
out=output[[1]]
coeff_sei=out[,1:28]
beta_mate_sei=out[,3:15]
beta_read_sei=out[,16:28]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_sette=mean(err)











#cross validation tra modello sei e sette
load('risultato_modello_biv_nove.rdata')
out=output[[1]]
coeff_sei=out[,1:36]
beta_mate_sei=out[,3:19]
beta_read_sei=out[,20:36]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_nove=mean(err)



#cross validation tra modello sei e sette
load('risultato_modello_biv_otto.rdata')
quantili=as.matrix(summary(output)$quantiles)
out=output[[1]]
coeff_sei=out[,1:32]
beta_mate_sei=out[,3:17]
beta_read_sei=out[,18:32]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_otto=mean(err)





#cross validation tra modello sei e sette
load('risultato_modello_biv_due.rdata')
out=output[[1]]
coeff_sei=out[,1:8]
beta_mate_sei=out[,3:5]
beta_read_sei=out[,6:8]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_due=mean(err)



#cross validation tra modello sei e sette
load('risultato_modello_biv_tre.rdata')
out=output[[1]]
coeff_sei=out[,1:12]
beta_mate_sei=out[,3:7]
beta_read_sei=out[,8:12]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_tre=mean(err)


#cross validation tra modello sei e sette
load('risultato_modello_biv_quattro.rdata')
out=output[[1]]
coeff_sei=out[,1:16]
beta_mate_sei=out[,3:9]
beta_read_sei=out[,10:16]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_quattro=mean(err)


load('risultato_modello_biv_cinque.rdata')
out=output[[1]]
coeff_sei=out[,1:20]
beta_mate_sei=out[,3:11]
beta_read_sei=out[,12:20]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  
  mate=Y[i,1]
  read=Y[i,2]
  err_iter=numeric(dim(coeff_sei)[1])
  for (g in 1:dim(coeff_sei)[1]){
    prev_mate=coeff_sei[g,1]+X[i,]%*%beta_mate_sei[g,]
    prev_read=coeff_sei[g,2]+X[i,]%*%beta_read_sei[g,]
    err_iter[g]=abs(mate-prev_mate)+abs(read-prev_read)
  }
  err[i]=mean(err_iter)
  print(i)
}
err_cinque=mean(err)
errors=c(mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei),mean(err_sette),mean(err_otto),mean(err_nove))
x11()
plot(errors,   col = "black", lwd = 5,xlab='models')