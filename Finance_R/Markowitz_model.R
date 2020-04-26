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


markowitz = function(x){
v = c()
w = matrix(nrow = x, ncol = length(assets))
for(i in 1:x){
  w[i, ] = runif(length(assets), 0, 1)
  w[i, ] = w[i, ]/sum(w[i, ])
  v[i] = t(w[i, ])%*%cov(ret)%*%w[i, ] 
}
  cat('A variância mínima é:', round(min(v), 4))
  cat('\n')
  pes = w[(which.min(v)), ]
  pes = t(pes)
  colnames(pes) = assets
  pes = data.frame(pes)
  
  return(pes)  
  
}

res = markowitz(20000)
res = t(res)

res[which.max(res), ]
res[which.min(res), ]



