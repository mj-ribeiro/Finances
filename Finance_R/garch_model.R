#*********************************************************************************
#                               GARCH
#*********************************************************************************

setwd("D:/DOUTORADO/Finance Econometry/Finance_aplications")


# Libraries

library(tseries)
library(timeSeries)
library(forecast)   # auto.arima
library(quantmod)
library(fGarch)
library(mFilter)
library(GetBCBData)   # get Bacen data


# GET DATA


ibov = getSymbols('^BVSP', src='yahoo', from= '2007-01-01', auto.assign = F)[,4]
colnames(ibov)= c('ibov')


ibov = ibov[is.na(ibov)==F]

#data = index(ibov)



#ib = as.vector(ibov) 
#ib = as.xts(ib, order.by = data)


#---------- Returns of ibov

ret = diff(log(ibov))
colnames(ret) = c('ret')

ret = ret[is.na(ret)==F]  # Drop na to work

# plot ret

par(mfrow=c(3,1))
plot(ret)
acf(ret)   # There isn't autocorrelation in returns. So isn't possible predict ret
pacf(ret)


# plot ret^2

par(mfrow=c(3,1))
plot(ret^2)
acf(ret^2)   # In squares of returns there is autocorrelation  
pacf(ret^2)


library(moments)

skewness(ret)  # the tail of left side is greater than tail of right side becase > 0

kurtosis(ret)  # distribution of ret is leptokurtic  because > 3


# histogram


par(mfrow=c(1,1))
hist(ret, breaks = 45, col = 'lightgreen')


#--------- Descriptive stats


basicStats(ibov)
basicStats(ret)



#------------ Plots

windows()
par(mfrow=c(3, 1))
plot(ibov, main='Evolution of IBOV')
plot(ret, main='Evolution of IBOV returns')
plot(ret^2, main='Evolution of square of IBOV returns')



#----------- Garch model

# Arch test


for (i in 1:5) {
  print(arch.test(x = ret, lags = i))
}



data = index(ret)
View(data)


# Estimation Garch (1, 1)

install.packages('ddpcr')
library(ddpcr)

garch1 = garchFit(formula = ~garch(1,1),
                     data = ret)
garch1

summary(garch1)

vol = garch1@sigma.t
vol = as.xts(vol, order.by = data)

mean(ret^2)
arima(ret^2, order = c(0,0,0))

#---------- Predict 



windows()
par(mfrow=c(2, 2))
plot(ibov, main='Evolution of IBOV')
plot(ret, main='Evolution of IBOV returns')
plot(ret^2, main='Evolution of square of IBOV returns')
plot(vol, main= 'Volatility of IBOV returns by GARCH(1,1)')




#------------ Other library


library(rugarch)


spec = ugarchspec()
spec

#------- Model

ugarchfit(variance.model = )

spec1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(1,0), include.mean=TRUE),  
                       distribution.model="norm")


garch2 = ugarchfit(spec = spec1, data= ret)

garch2



#--------- Predict


pred2 = ugarchforecast(garch2, n.ahead = 5)
pred2 = pred2@forecast$sigmaFor

plot(pred2, type='l')










