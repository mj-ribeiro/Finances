library(quantmod)



#----------- My function to get data


get_data = function(x, d= "2000-01-01"){
  z = getSymbols(x, src='yahoo', 
                 from= d, 
                 #periodicity = "monthly",    
                 auto.assign = F)[,4]
  return(z)
}


assets = c('^BVSP', 'BBAS3.SA')




df = data.frame( lapply(assets, get_data))

colnames(df) = c('IBOV', 'BBAS3')


ret =  diff(log (df$IBOV) )


plot(ret, type='l')






library(propagate)


fitDistr(ret)



