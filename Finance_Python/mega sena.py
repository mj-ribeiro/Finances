# -*- coding: utf-8 -*-
"""
Created on Wed Dec 25 18:50:07 2019

@author: Marcos J. Ribeiro
"""
##MARCOS J RIBEIRO

import numpy as np
import random as rd
import seaborn as sb
import matplotlib.pyplot as plt


l = np.arange(1,61,1)

m = np.zeros(6)

v = []
c = 0

for i in range(1000):
    c += 1
    for i in range(6):
        m[i] = rd.choice(l)
        v.append(m[i])
    print(f'\033[1;033m Jogo {c} **** {m}')      
        


plt.figure(figsize=(12,8))
plt.hist(v, bins=60)
plt.xlabel('Números sorteados', fontsize=20)
plt.ylabel('Frequência dos números sorteados', fontsize=20)





n = 100000
m = np.zeros((n, 6))
v = []
c = 0

for j in range(n):
    c += 1
    for i in range(6):
        m[j, i] = rd.choice(l)
        #v.append(m[i])
    print(f'\033[1;033m Jogo {c} **** {m[j]}')   
    

mj = np.array((60, 17, 10, 3, 33, 15))

g = 0
for j in range(n):
    tf = np.array_equal(mj, m[j])
    if tf == True:
        g += 1
        print('vc ganhou!!!')
    else:
        print('vc perdeu kkkk')



print(f'Seu número de acertos é: {g}')


















import math as mt

cb = mt.factorial(60)/ (mt.factorial(6)*mt.factorial(60-6))

for i in range(105):
    jogos = i
    prob = jogos/cb
    print(f'\033[1;033mA probabilidade de ganhar na MEGA SENA com {jogos} jogos é {prob*100:.9f}%')




prob =0
jg = 0
while prob<0.1:
    #for i in range(1000000):
    jg += 1
    prob = jg/cb
    print(f'\033[1;033mA probabilidade de ganhar na MEGA SENA com {jg} jogos é {prob*100:.9f}%')

























