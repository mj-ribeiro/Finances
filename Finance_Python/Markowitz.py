## Marcos J Ribeiro

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt



ret = pd.read_excel(input('what is the file path on your computer? (Use xlsx file.)'))
ret.set_index('data', inplace=True) 


def markowitz(x):
    na = len(ret.columns)
    print('\033[1;033m')
    pes = []
    pret = []
    vol = []
    for i in range(x):
        w = np.random.random(na)
        w /= np.sum(w)
        pes.append(w)
        vol.append(np.sqrt(np.dot(w.T, np.dot(ret.cov(), w))))
        pret.append(np.sum(w*ret.mean()))
    pes = np.array(pes)
    pret = np.array(pret)
    vol = np.array(vol)
    vmin = np.min(vol)  
    pos = np.argmin(vol) 
    pmin = pes[pos]
    retp = pret[pos]
    print('-='*20)
    print('{:>30}'.format('MODELO DE MARKOWITZ'))
    print('-='*20)
    print(f'A variância mínima do portfólio é: {vmin:.4f}')
    print(f'O retorno do portfólio de variância mínima é: {retp:.4f}')
    print(f'A soma dos pesos é: {np.sum(pmin):.2f}')
    fig, ax = plt.subplots()
    ax.scatter(vol, pret)
    ax.set_xlabel('Risco')
    ax.set_ylabel('Retorno')
    ax.set_title('Fronteira Eficiente')
    print('-='*20)
    print('ATIVOS {:>30}'.format('PESOS'))
    print('-='*20)
    for i in range(na):
        print(f'{ret.columns[i]:.<30} {pmin[i]:.4f}')
    

   

