
rm(list = ls())

#cross validation tra modello sei e sette
load('LM1.rdata')
out=output[[1]]
beta_mate_uno=out[,1:6]
inter=out[,7]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate_uno[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_uno=mean(err)


#cross validation tra modello sei e sette
load('LM2.rdata')
out=output[[1]]
beta_mate=out[,1:9]
inter=out[,10]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_due=mean(err)






#cross validation tra modello sei e sette
load('LM3.rdata')
out=output[[1]]
beta_mate=out[,1:11]
inter=out[,12]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_tre=mean(err)













#cross validation tra modello sei e sette
load('LM4.rdata')
out=output[[1]]
beta_mate=out[,1:13]
inter=out[,14]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_quattro=mean(err)



#cross validation tra modello sei e sette
load('LM5.rdata')
out=output[[1]]
beta_mate=out[,1:15]
inter=out[,16]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_cinque=mean(err)





#cross validation tra modello sei e sette
load('LM6.rdata')
out=output[[1]]
beta_mate=out[,1:17]
inter=out[,18]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_sei=mean(err)






#cross validation tra modello sei e sette
load('LM7.rdata')
out=output[[1]]
beta_mate=out[,1:19]
inter=out[,20]
#it=400
it=N
err=numeric(it)
for (i in 1:it){
  mate=Y[i]
  err_iter=numeric(length(inter))
  for (g in 1:length(inter)){
    
    prev_mate=inter[g]+X[i,]%*%beta_mate[g,]
    
    err_iter[g]=abs(mate-prev_mate)  
  }
  err[i]=mean(err_iter)
  print(i)
}
err_sette=mean(err)






errors=c(mean(err_uno),mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei),mean(err_sette))
x11()
plot(errors, col = "black", lwd = 5,xlab='models')




