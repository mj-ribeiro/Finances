#***********************************************************************************************
#                                   Algorithms
#***********************************************************************************************

setwd("D:/Git projects/Finance/Finance_R")

library(caret)
library(ROSE)






# see: http://topepo.github.io/caret/train-models-by-tag.html#neural-network
# see: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
# CV in TS https://rpubs.com/crossxwill/time-series-cv


#--- load variables

pca = readRDS('pca.rds')
df = readRDS('df.rds')


table(df$x)


#---- Control train


control_train = trainControl(method = 'repeatedcv', number = 10, repeats = 2)    # ten fold


#----- Neural net

#control_train =trainControl(method = "timeslice",initialWindow = 36, horizon = 12, fixedWindow = T,allowParallel = T)  


model4 = train(as.factor(x) ~., data=df, trControl = control_train, 
               method='nnet', threshold = 0.3)


model4

confusionMatrix(model4)


#------ Multilogit


model3 = train(as.factor(x) ~., data=df, 
               trControl = control_train, 
               method='multinom', family='binomial') 

model3
confusionMatrix(model3)


#------- SVM

model5 = train(as.factor(crise) ~., data=df, trControl = control_train, 
               method='svmRadial') 

model5
confusionMatrix(model5)




#------- KNN


model6 = train(as.factor(crise) ~., data=df, 
               trControl = control_train, 
               method='knn') 

model6
confusionMatrix(model6)








