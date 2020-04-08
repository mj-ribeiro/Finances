#*********************************************************************************
#                               Value at Risk
#*********************************************************************************

setwd("D:/DOUTORADO/Finance Econometry/Finance_aplications")


# Libraries

library(tseries)
library(timeSeries)
library(forecast)   # auto.arima
library(quantmod)
library(fGarch)
library(mFilter)


# GET DATA


ibov = getSymbols('^BVSP', src='yahoo', from= '2007-01-01', auto.assign = F)[,4]
colnames(ibov)= c('ibov')


ibov = ibov[is.na(ibov)==F]


#---------- Returns of ibov


ret = diff(log(ibov))
colnames(ret) = c('ret')

ret = ret[is.na(ret)==F]  # Drop na to work

ret = as.vector(ret)


# Basic stats

basicStats(ret)


# Histogram


q5 = quantile(ret, 0.05)     # VaR 5%

hist(ret, breaks = 30, col = 'lightgreen')
abline(v=q5)
text(x= q5-0.01, y=900, labels = 'VaR')


esf = mean(ret[ret<q5])      # Expected Shortfall




#---------- Value at risk & Expected Shortfall (Bootstrap)


var = c()
es = c()

for (i in 1:5000){
  amostra = sample(ret, 1500)
  var[i] = quantile(amostra, 0.05)         # VaR
  es[i] = mean(amostra[amostra<var[i]])    # espected shortfall
}


# Confidence interval 

t.test(var)

t.test(es)



















