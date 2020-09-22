# -*- coding: utf-8 -*-
"""
Created on Tue Feb  4 20:04:14 2020

@author: Marcos J Ribeiro
"""
from pandas_datareader import data as wb
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import itertools as itl






def markowitz(x, assets):
    pf_data = pd.DataFrame()

    
    for a in assets:
        pf_data[a] = wb.DataReader(a, data_source='yahoo', start='2018-1-1')['Adj Close']


    
    ret = np.log(pf_data / pf_data.shift(1) )*100
    ret.drop(ret.index[0], inplace=True)
    
    
    
    na = len(ret.columns)
    print('\033[1;033m')
    pes = np.zeros((x, na))
    ret_p = np.zeros(x)
    var = np.zeros(x)
        
    covar = ret.cov()
    
    for i in itl.count():
               
        if i < x:
            w = np.random.random(na)
            w = w/ np.sum(w)
            pes[i] = w
            var[i] = (np.sqrt(np.dot(w.T, np.dot(covar, w))))     #CALCULATE THE VARIANCE
            ret_p[i]= (np.sum(w*ret.mean()))                      #CALCULATE THE RETURNS
        else:
            break
   
    var_min = np.min(var)    # GET THE MINIMUN VALUE OF VARIANCE
    pos = np.argmin(var)     # GET THE POSITION OF MINIMUN VALUE OF VARIANCE
    pes_min = pes[pos]       # GET THE POSITION OF WEIGHTS ASSOCIATED WITH MINIMUN VALUE OF VARIANCE
    ret_var_min = ret_p[pos] # GET THE RETURN OF PORTFOLIO OF MINIMUM VARIANCE
    
    
    print('-='*20)
    print('{:>30}'.format('MODELO DE MARKOWITZ'))
    print('-='*20)
    print(f'A variância mínima do portfólio é: {var_min:.4f}')
    print(f'O retorno do portfólio de variância mínima é: {ret_var_min:.4%}')
    print(f'A soma dos pesos é: {np.sum(pes_min):.2%}')
   
    fig, ax = plt.subplots()
    ax.scatter(var, ret_p)
    ax.set_xlabel('Risco')
    ax.set_ylabel('Retorno')
    ax.set_title('Fronteira Eficiente')
    
    print('-='*20)
    print('ATIVOS {:>30}'.format('PESOS'))
    print('-='*20)
    for i in range(na):
        print(f'{ret.columns[i]:.<31} {pes_min[i]:.4%}')
    print('-='*20)

    

    






assets = ['B3SA3.SA', 'BBAS3.SA']
markowitz(100000, assets)


 






