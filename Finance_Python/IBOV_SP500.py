# -*- coding: utf-8 -*-
"""
Created on Wed Feb  5 21:18:52 2020

@author: Marcos J Ribeiro
"""



from pandas_datareader import data as wb
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm



assets = ['^GSPC', '^BVSP']
    

df = pd.DataFrame()

for a in assets:
    df[a] = wb.DataReader(a, data_source='yahoo', start='2000-1-1')['Adj Close']
    

df.rename(columns={'^GSPC':'SP500', '^BVSP': 'IBOV'},  inplace= True )


df.dropna(inplace=True)

IBOV = df['IBOV']
SP500 = df['SP500']





#------------ REGRESSION


reg = sm.OLS(IBOV, SP500).fit()


print(f'\033[1;033m {reg.summary()}')



np.corrcoef(IBOV, SP500)


#---------------- ARIMA

arma_mod = sm.tsa.ARMA(IBOV, order=(2,2)).fit()
print(f'\033[1;033m {arma_mod.summary()}')



#--------------- PLOTS   


from sklearn.preprocessing import scale
S_IBOV = scale( IBOV, axis=0, with_mean=True, with_std=True, copy=True )
S_SP500 = scale( SP500, axis=0, with_mean=True, with_std=True, copy=True )



plt.plot(S_IBOV)
plt.plot(S_SP500)


























