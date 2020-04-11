#**************************************************************************************
#                              Volatility index
#****************************************************************************************

setwd("D:/Git projects/Finance/Finance_R")


#------------- CMAX Function

# w é o tamanho da janela
# é a quantidade de janelas
# s é o vetor que vou passar a função


CMAX = function(w, n, s){
  
  l = matrix(nrow=n,ncol = (w+1))
  max = matrix(nrow=n, ncol = 1)
  cmax = matrix(nrow=n, ncol = 1)
  
  for (j in 1:n){
    
    l[j, 1:(w+1)] = s[j:(w+j)]
    max[j] = max(l[j, 1:(w+1)])
    
    cmax[j] = l[j, (w+1)]/max(max[j])
  }
  return(cmax)
}





# Libraries

library(tseries)
library(timeSeries)
library(quantmod)
library(fGarch)
library(GetBCBData)


ibov = getSymbols('^BVSP', src='yahoo', 
                  from= '2000-01-01', 
                  periodicity = "monthly",    # IBOV mensal
                  auto.assign = F)[,4]


colnames(ibov) = 'ibov'
ibov = ibov[is.na(ibov)==F]

# VIX

vix = getSymbols('^VIX', src='yahoo', 
                 periodicity = "monthly",
                 from= '2000-01-01', 
                 auto.assign = F)[,4]

colnames(vix) = 'vix'




# 11768 - Índice da taxa de câmbio real (INPC)


cb = gbcbd_get_series(11768, first.date= '2000-01-01',  
                      format.data = "long", be.quiet = FALSE)[ ,1:2]

data = cb$ref.date
cb[,1]=NULL
cb = xts(cb, order.by = data)

rownames(cb) = data    # colocar a data como índice



# cdi

cdi = gbcbd_get_series(4391, first.date= '2000-01-01',  
                       format.data = "long", be.quiet = FALSE)[ ,1:2]

data = cdi$ref.date
cdi[,1]= NULL
cdi = xts(cdi, order.by = data)

rownames(cdi) = data    # colocar a data como índice




# PTAX

ptax = getSymbols('BRL=X', src='yahoo', 
                  from= '2000-01-01', 
                  periodicity = "monthly",
                  auto.assign = F)[,4]

#-------- Descriptive stats


df = data.frame(cb, ibov[index(cb)], vix[index(cb)] )

apply(df[,1:3], 2, basicStats)




#plots

windows()
par(mfrow=c(1,3))
plot(ibov)
plot(vix)
plot(cb, type='l')




# returns

ret = diff(log(ibov))
basicStats(ret)

ret = ret[is.na(ret)==F]


#ret = matrix(ret)

6/144
v = c(1, 3, 144, 5, 6)
CMAX(2, 4, v)

#------ Using CMAX function


cm2 = CMAX(23, (length(ibov)-24), ibov)


hist(cm2, breaks = 35, col='lightgreen', 
     main='Histograma para o CMAX diário \n com 24 janelas')

 

#lim = mean(cm2)-2*sd(cm2)
lim = 0.7

cm2[cm2<lim]
sum((cm2<lim)*1)   # count 


# get the data of ibov

data = index(ibov)
data1 = data[25:length(ibov)]



# transform cmax in xts object

cmts = xts(x=cm2, order.by = data1) 


# Plot CMAX - 24

library(zoo)

windows()
par(mfrow=c(1,1))
plot(as.zoo(cmts), main = 'CMAX- W24', ylim=c(0.5, 1),
     type='l', ylab='CMAX', xlab='Ano')
#xaxt='n'
#axis(1, at=seq(20, 200, 10), labels=seq(2002, 2020, 1))
abline(h=lim)
text(as.Date('2008-10-10'), y=0.55, labels = 'Crise \n de 2008', cex=0.8) 
text(as.Date('2020-03-10'), y=0.52, labels = 'Crise \n do COVID-19', cex=0.8) 
text(as.Date('2000-03-10'), y=0.6, labels = 'Bolha da \n internet', cex=0.8) 
text(as.Date('2001-12-9'), y=0.7, labels = '11 de setembro', cex=0.8) 


par(mfrow=c(1,2))
plot(ret)
plot(cmts)

# Value at Risk to CMAX

VaR1 = quantile(cmts, 0.05)
VaR2 = quantile(cmts, 0.01)

par(mfrow=c(1,1))
hist(cm2, breaks = 35, col='lightgreen', probability = T, 
     main='Histograma para o CMAX mensal \n com 24 janelas')
abline(v=VaR1)
abline(v=VaR2)
abline(v=med)




#----- Create Dummy


crise = matrix(nrow = length(cmts))

crise = ifelse(cm2<lim, 1, 0)  # definition of crise


View(crise)
sum((crise==1)*1)


crise = xts(crise, order.by = data1)
plot(crise)


pos = which(crise==1)   # pegar a posição onde crise== 1
pos


#crise[(13-12):13] = 1   # gambiarras haha


for (c in 7:length(pos)){
  crise[(pos[c]-12):pos[c]] = 1
}
pos
which(pos==170)

crise[(pos[9]+1):21]=2
crise[(pos[10]+1):(pos[10]+12)]=2
crise[(pos[11]+1):(pos[11]+12)]=2
crise[(pos[12]+1):(pos[12]+12)]=2
crise[(pos[13]+1):(pos[13]+12)]=2
crise[(pos[14]+1):(pos[14]+12)]=2
crise[(pos[15]+1):(pos[15]+12)]=2
crise[(pos[16]+1):(pos[16]+12)]=2
crise[(pos[17]+1):(pos[17]+12)]=2
crise[(pos[18]+1):(pos[18]+12)]=2
crise[(pos[19]+1):(pos[19]+12)]=2
crise[pos[20]] = 2



prop.table(table(crise))







# Pie Graph

pie(table(crise), radius = 1)
text(locator(n=1),
     paste(round(prop.table(table(crise))[1],
                 digits=2)*100,"%"))
text(locator(n=1),
     paste(round(prop.table(table(crise))[2],
                 digits=2)*100,"%"))
text(locator(n=1),
     paste(round(prop.table(table(crise))[3],
                 digits=2)*100,"%"))



#----

data = index(cmts)

crise = xts(crise, order.by = data)
cb = cb[data]
vix = vix[data]


data = index(cb)
vix = vix[data]
crise = crise[data] 
cdi = cdi[data]


# transform data in data frame

df = data.frame(vix, cb, crise, cdi)

df = data.frame(date=index(index(cb)), coredata(df))

df$date = NULL



#---- Algorithm

# see: http://topepo.github.io/caret/train-models-by-tag.html#neural-network


# as classes são desbalanceadas
# logo vou retirar uma parcela da amostra

# see: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

#library(imbalance)

#d = mwmote(df, numInstances = 500, classAttr = "x")



library(neuralnet)
d = df[1:192, ]
d_test = df[193:218, ]

NN = neuralnet(x~., d, hidden = 3, linear.output = T)
plot (NN)
prev = predict(NN, d_test)
prev = ifelse(prev>0.5, 1,0)

c = table(d_test[,3], prev)
confusionMatrix(c)

which(vix==13.54)
df



# CV in TS https://rpubs.com/crossxwill/time-series-cv

library(caret)   # library to cross validation 


# Neural net
control_train =trainControl(method = "timeslice",
                            initialWindow = 36,
                            horizon = 12,
                            fixedWindow = T,
                            allowParallel = T)
                            
model4 = train(as.factor(x) ~., data=df, trControl = control_train, method='nnet', threshold = 0.3)
               

model4

confusionMatrix(model4)


# Multilogit

control_train = trainControl(method = 'repeatedcv', number = 10, repeats = 2)    # ten fold
model3 = train(as.factor(x) ~., data=df, trControl = control_train, method='glm', family='binomial') 

model3
confusionMatrix(model3)


# SVM

control_train = trainControl(method = 'cv', number = 10)    # ten fold
model5 = train(as.factor(x) ~., data=df, trControl = control_train, method='svmRadial') 

model5
confusionMatrix(model5)




# KNN

control_train = trainControl(method = 'cv', number = 10)    # ten fold
model6 = train(as.factor(x) ~., data=df, trControl = control_train, method='knn') 

model6


confusionMatrix(model6)





