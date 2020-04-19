#**************************************************************************************
#                              PCA with volatility index
#****************************************************************************************

setwd("D:/Git projects/Finance/Finance_R")

#--------- Libraries

library(tseries)
library(timeSeries)
library(fGarch)
library(quantmod)



#----------- Get data

get_data = function(x){
  z = getSymbols(x, src='yahoo', 
                  from= '2002-01-01', 
                  periodicity = "monthly",    
                  auto.assign = F)[,4]
  return(z)
}


#-------- Getting assets

assets = c('PETR4.SA', 'ITUB4.SA', 'VALE3.SA',  
           'PETR4.SA', 'BBAS3.SA', 'VIVT4.SA', 
           'GGBR4.SA', 'RADL3.SA')


df = data.frame(lapply(assets, get_data))


nl = sum((df[,1]>0)*1)   # count row number

df = df[-nl, ] # droplast row

nl = sum((df[,1]>0)*1)


# create data sequence

data = seq(as.Date("2001-1-1"), as.Date("2020-4-1"), by = "month")


#-------- Calculating retunrs


ret = data.frame(matrix(nrow =(nl-1), ncol=length(assets)))



for(i in 1:length(assets)){
  ret[ ,i] = diff(log(df[,i]))
}


colnames(ret) = assets



#------------- Apllying PCA algorithm


library(caret)  

pca = preProcess(x= ret, method = 'pca', pcaComp = 1)
pca = predict(pca, ret)


ret = xts(ret, order.by = data[-1])
pca = xts(pca, order.by = data[-1])


# Plot first component

plot(pca$V1, type='l')








viv = get_data('RADL3.SA')

index(viv)[2]





