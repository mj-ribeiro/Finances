#**********************************************************************************************
#                               My portfolio
#**********************************************************************************************

setwd("D:/Git projects/Finance/Finance_R")


library(ggplot2)
library(quantmod)
library(reshape2)



assets = c('^BVSP')


get = function(x, d= "2018-03-01"){
z = getSymbols(x, src='yahoo', 
               from= d, 
               periodicity = "monthly",    
               auto.assign = F)[,4]
    return(z)
}



# get IBOV

df = data.frame(lapply(assets, get))

colnames(df) = 'ibov'


ret = diff(log(df$ibov))


# get my portfolio returns


my_p = read.csv('portfolio.csv', sep = ';', header = T)[1:2] 

my_p$rentabilidade = my_p$rentabilidade/100

my_p$ret = ret


colnames(my_p) = c('data', 'Minha carteira', 'IBOV')  
  
  
# make graph  

df4 <- melt(data = my_p, id.vars = "data")
colnames(df4) =  c('data', 'Fator', 'Rendimento')

df4$data =  as.Date(df4$data) 

ggplot(df4, aes(x = data, y = Rendimento)) + 
  scale_x_date(date_labels="%m/%Y",date_breaks  ="2 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=20), 
        axis.text.y = element_text(size=20), 
        axis.title.x = element_text(colour = 'black', size=21),
        axis.title.y = element_text(colour = 'black', size=21),
        legend.title=element_blank(),
        legend.text = element_text(colour="black", size = 21),
        legend.position="bottom" ) + 
  geom_line(aes(color = Fator, linetype = Fator), size=1) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  xlab('Anos') + 
  ylab('Rendimento') 




