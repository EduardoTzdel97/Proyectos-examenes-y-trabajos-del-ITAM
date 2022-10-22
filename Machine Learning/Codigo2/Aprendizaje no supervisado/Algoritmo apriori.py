# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 12:01:08 2020

@author: Edu
"""


    
import pandas as pd
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori, association_rules


dataset = [['Milk', 'Onion', 'Nutmeg', 'Kidney Beans', 'Eggs', 'Yogurt'],
           ['Dill', 'Onion', 'Nutmeg', 'Kidney Beans', 'Eggs', 'Yogurt'],
           ['Milk', 'Apple', 'Kidney Beans', 'Eggs'],
           ['Milk', 'Unicorn', 'Corn', 'Kidney Beans', 'Yogurt'],
           ['Corn', 'Onion', 'Onion', 'Kidney Beans', 'Ice cream', 'Eggs']]




te = TransactionEncoder()
te_ary = te.fit(dataset).transform(dataset)
df = pd.DataFrame(te_ary, columns=te.columns_)


# Genera los itemsets 
items_frecuentes = apriori(df, min_support=0.4, use_colnames=True)


# Genera las reglas 
reglas = association_rules(items_frecuentes, metric ="confidence", min_threshold = 0.6) 
reglas = reglas.sort_values(['confidence'], ascending =[False]) 
print(reglas.head()) 






