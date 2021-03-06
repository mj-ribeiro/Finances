#************************************************************************************
#                               MARKOWITZ MODEL
#************************************************************************************

setwd("D:/Git projects/Finance/Finance_R")



library(tseries)
library(timeSeries)
library(fGarch)
library(quantmod)



#----------- My function to get data


get_data = function(x, d= "2000-01-01"){
  z = getSymbols(x, src='yahoo', 
                 from= d, 
                 periodicity = "monthly",    
                 auto.assign = F)[,4]
  return(z)
}


#************ Assets that i want

assets = c("VALE3.SA", "PETR4.SA", "PETR3.SA", "ABEV3.SA", "BBAS3.SA", "ITSA4.SA",
"LREN3.SA","BBDC3.SA", "BRFS3.SA", "WEGE3.SA", "RADL3.SA", "VIVT4.SA", "SBSP3.SA",
"LAME4.SA","EMBR3.SA", "CMIG4.SA", "VVAR3.SA", "ELET3.SA", "CSNA3.SA", "CYRE3.SA",
"ENBR3.SA","USIM5.SA", "HGTX3.SA")



df = data.frame(lapply(assets, get_data))


#******* replace nas by mean

for(i in 1:length(df)){
  df[ ,i] = ifelse(is.na(df[,i]), 
                   (mean(df[,i], na.rm = T)),
                   df[,i] )
}

basicStats(df)


nl = sum((df[,1]>0)*1)

df = df[-nl,]

nl = sum((df[,1]>0)*1)


#*********** calculating returs



ret = data.frame(matrix(nrow =(nl-1), ncol=length(assets)))



for(i in 1:length(assets)){
  ret[ ,i] = diff(log(df[,i]))
}

colnames(ret) = assets




#*********** Algorithm
mean_ret = c()

for (j in 1:length(assets)){
  mean_ret[j] = mean(ret[ ,j])
}
  
  
markowitz = function(x){
  v = c()
  e = c()
  w = matrix(nrow = x, ncol = length(assets))
for(i in 1:x){
  w[i, ] = runif(length(assets), 0, 1)
  w[i, ] = w[i, ]/sum(w[i, ])
  v[i] = t(w[i, ])%*%cov(ret)%*%w[i, ] 
  e[i] = w[i, ]%*%mean_ret
}
  plot(v, e, col='blue', xlab='Variance', ylab='Expected return', pch=20)
  cat('A vari�ncia m�nima �:', round(min(v), 4))
  
  cat('\n')
  pes = w[(which.min(v)), ]
  pes = t(pes)
  colnames(pes) = assets
  pes = data.frame(pes)
  
  return(pes)  
  
}

res = markowitz(10000)
res = t(res)

row.names(res) = assets


res[which.max(res), ]
res[which.min(res), ]

#plot(rnorm(10), rnorm(10), pch=20, las=1, type='p', col='red', bty='l')

100*res

#------- Using Rsolnp library


library('Rsolnp')


markowitz2 = function(w){
  v = t(w)%*%cov(ret)%*%w 
  return(v)  
}

eq = function(w){
  sum(w)
}


w = runif(length(assets), 0, 1)
w = w/sum(w)

res = solnp(w,      #starting values 
      markowitz2,   #function to optimise
      eqfun=eq,     #equality function 
      eqB=1,        #the equality constraint. Obviously is one
      LB=c(rep(0, length(assets))), #lower bound for parameters i.e. greater than zero
      UB=c(rep(1, length(assets)))) #upper bound for parameters i.e less than one


v1 = res$values[3]

sum(res$pars)

pes = res$pars


for(i in 1:length(assets)){
  cat('\033[1;035m')
  cat(assets[i],'-------', round(pes[i], 5))
  cat('\n')
}


v1 = res$values[length(res$values)]

cat('A vari�ncia do modelo � dada por:', round(v1,6))



res1 = data.frame(assets, pes)
View(res1)


print(res1)


