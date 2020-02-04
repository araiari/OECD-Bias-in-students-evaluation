
rm(list = ls())








#cross validation tra modello sei e sette
load('mod2_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_due=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  
  prev_mate=inter+quantili[gr]+X[i,]%*%quantili['theta']
  
  err_due[i]=abs(mate-prev_mate)
}














#cross validation tra modello sei e sette
load('mod4_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_quattro=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  
  prev_mate=inter+quantili[gr]+X[i,]%*%c(quantili['theta[1]'],quantili['theta[2]'])
  
  err_quattro[i]=abs(mate-prev_mate)
}





















#cross validation tra modello sei e sette
load('mod1_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_uno=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  id=M*6+gr
  ga=c(quantili[gr],quantili[gr+M],quantili[gr+2*M],quantili[gr+3*M],quantili[gr+4*M],quantili[gr+5*M])
  prev_mate=inter+quantili[id]+X[i,]%*%c(quantili['theta'])+Z[i,]%*%ga
  
  err_uno[i]=abs(mate-prev_mate)
}





#cross validation tra modello sei e sette
load('mod3_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_tre=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  id=M*6+gr
  ga=c(quantili[gr],quantili[gr+M],quantili[gr+2*M],quantili[gr+3*M],quantili[gr+4*M],quantili[gr+5*M])
  prev_mate=inter+quantili[id]+X[i,]%*%c(quantili['theta[1]'],quantili['theta[2]'])+Z[i,]%*%ga
  
  err_tre[i]=abs(mate-prev_mate)
}








#cross validation tra modello sei e sette
load('mod5_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_cinque=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  id=M*13+gr
  ga=c(quantili[gr],quantili[gr+M],quantili[gr+2*M],quantili[gr+3*M],quantili[gr+4*M],quantili[gr+5*M],quantili[gr+6*M],quantili[7*gr],quantili[8*gr],quantili[9*gr],quantili[10*gr],quantili[11*gr],quantili[12*gr])
  prev_mate=inter+quantili[id]+X[i,]%*%c(quantili['theta'])+Z[i,]%*%ga
  
  err_cinque[i]=abs(mate-prev_mate)
}





#cross validation tra modello sei e sette
load('mod6_nonstand_congamma.Rdata')
quantili=as.matrix(summary(output)$quantiles)[,3]
inter=quantili['theta0']
err_sei=rep(0,N)
for (i in 1:N){
  gr=g[i]
  mate=Y[i]
  id=M*13+gr
  ga=c(quantili[gr],quantili[gr+M],quantili[gr+2*M],quantili[gr+3*M],quantili[gr+4*M],quantili[gr+5*M],quantili[gr+6*M],quantili[7*gr],quantili[8*gr],quantili[9*gr],quantili[10*gr],quantili[11*gr],quantili[12*gr])
  prev_mate=inter+quantili[id]+X[i,]%*%c(quantili['theta[1]'],quantili['theta[2]'])+Z[i,]%*%ga
  
  err_sei[i]=abs(mate-prev_mate)
}





errori=c(mean(err_uno),mean(err_due),mean(err_tre),mean(err_quattro),mean(err_cinque),mean(err_sei))

plot(errori, col = "black", lwd = 5)
