# -*- coding: utf-8 -*-
"""
Created on Mon Jan 27 10:21:29 2020

@author: Marcos J Ribeiro
"""


print('\033[1;033m-='*37) 
print('{:>47}'.format('Conversor de taxas'))
print('-='*37)                

p = str(input('Digite play para começar! '))
while p not in 'play':
    p = str(input('Digite play para começar! '))
    if p in 'play':
        continue   
n = ' '
while True:
    try:     
        tx = float(input('\033[1;033mQual é a taxa de juros que você quer converter? '))
        q = str(input('É anual, mensal ou diária? '))
        while q not in 'anual, mensal, diária':
            q = str(input('É anual, mensal ou diária? '))
        
        c = str(input('Você quer converter para mensal, diária, ou anual? '))  
        while c not in 'anual, mensal, diária':
            c = str(input('Você quer converter para mensal, diária, ou anual? '))  

        if q == 'anual' and c=='mensal':
            tx_n = (1+tx)**(1/12)-1
            print(' '*2)
            print(f'A taxa {q} {tx} convertida para {c} é igual a: {tx_n:.4%}')
            
        elif q == 'anual' and c=='diária':
            tx_n = (1+tx)**(1/365)-1
            print(' '*2)
            print(f'A taxa {q} de {tx} convertida para {c} é igual a: {tx_n:.4%}')
        
        elif q == 'mensal' and c=='diária':
            tx_n = (1+tx)**(1/30)-1
            print(' '*2)
            print(f'A taxa {q} de {tx} convertida para {c} é igual a: {tx_n:.4%}')
        
        elif q == 'mensal' and c=='anual':
            tx_n = (1+tx)**(12)-1
            print(' '*2)
            print(f'A taxa {q} de {tx} convertida para {c} é igual a: {tx_n:.4%}')
        
        elif q == 'diária' and c=='anual':
            tx_n = (1+tx)**(365)-1
            print(' '*2)
            print(f'A taxa {q} de {tx} convertida para {c} é igual a: {tx_n:.4%}')

        elif q == 'diária' and c=='mensal':
            tx_n = (1+tx)**(30)-1
            print(' '*2)
            print(f'A taxa {q} de {tx} convertida para {c} é igual a: {tx_n:.4%}')

        
        n = str(input('quer continuar?'))
        while n not in 'sim, Sim, Não, não':
            n = str(input('quer continuar? [Sim/Não]'))
        
        if n in 'não, Não':
            break
        elif n in 'sim, Sim':
            continue
    except (ValueError, TypeError):
        print('Houve um problema com o tipo de dado que você digitou.\n Tente novamente!')
    except ZeroDivisionError:
        print('Não é possível dividir um número por zero!\n Tente novamente!')
    except SyntaxError:
        print('Erro de sintaxe. \n Tente novamente.')

print('\033[1;033m-='*37) 
print('{:*^74}'.format('FIM DO PROGRAMA'))
print('-='*37)                





