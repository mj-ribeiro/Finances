# -*- coding: utf-8 -*-
"""
Created on Wed Jan  8 12:31:16 2020

@author: Marcos J Ribeiro
"""


from pandas_datareader import data as wb
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import itertools as itl




    

def konno(x, assets):
    pf_data = pd.DataFrame()

    for a in assets:
        pf_data[a] = wb.DataReader(a, data_source='yahoo', start='2018-1-1')['Adj Close']
    
    
        
    ret = np.log(pf_data / pf_data.shift(1) )*100
    ret.drop(ret.index[0], inplace=True)
    
    na = len(ret.columns)
    
    pes = []
    pret = []
    vol = []
    
    for i in range(x):
        w = np.random.random(na)
        w /= np.sum(w)
        pes.append(w)
        vol.append(np.mean(np.abs(np.dot(ret, w)-np.mean(np.dot(ret, w))))) 
        pret.append(np.sum(w*ret.mean()))
        
    pes = np.array(pes)
    pret = np.array(pret)
    vol = np.array(vol)
    vmin = np.min(vol)  
    pos = np.argmin(vol) 
    pmin = pes[pos]
    retp = pret[pos]
    
    print('\033[1;033m')
    print('-='*20)
    print('{:>30}'.format('MODELO DE KONNO & YAMAZAKI'))
    print('-='*20)
    print(f'A variância mínima do portfólio é: {vmin:.4f}') 
    print(f'O retorno do portfólio de variância mínima é: {retp:.4%}')
    print(f'A soma dos pesos é: {np.sum(pmin):.2%}')
    
    fig, ax = plt.subplots()
    ax.scatter(vol, pret)
    ax.set_xlabel('Risco')
    ax.set_ylabel('Retorno')
    ax.set_title('Fronteira Eficiente (Konno & Yamazaki)')
    
    print('-='*20)
    print('ATIVOS {:>30}'.format('PESOS'))
    print('-='*20)
    for i in range(na):
        print(f'{ret.columns[i]:.<30} {pmin[i]:.4%}')
    print('-='*20)

    
assets = ['BBAS3.SA', 'B3SA3.SA']

konno(100000, assets) 








   
