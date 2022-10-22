# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 11:54:50 2020

@author: Edu
"""

import matplotlib.pyplot as plt
from scipy.stats import multivariate_normal
from sklearn.mixture import GaussianMixture
from sklearn import datasets

 

X, y = datasets.load_iris(return_X_y=True)

 
### Código de EM
GMM = GaussianMixture(n_components=3).fit(X)

 

# Datos de entrenamiento  
plt.scatter(X[:, 0], X[:, 2], c=y, cmap='Paired',s=50)
plt.show()

 

# Clusters calculados 
color = 0
colores = ['blue', 'red', 'black', 'orange', 'gray', 'navy', 'turquoise', 'green']
muestras = 100

 

for m,c in zip(GMM.means_,GMM.covariances_):
    multi_normal = multivariate_normal(mean=m,cov=c)
    puntos = multi_normal.rvs(size=muestras, random_state=0)
    plt.scatter(puntos[:, 0], puntos[:, 2], c=colores[color], s=10)
    plt.scatter(m[0],m[2],c=colores[color],zorder=10,s=200)
    color+=1
    
plt.show()


