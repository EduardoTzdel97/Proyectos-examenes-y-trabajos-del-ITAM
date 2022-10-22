# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 10:53:26 2020

@author: Mesa Juntas
"""

import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn import datasets

X, y = datasets.load_breast_cancer(return_X_y=True)


xNew = StandardScaler().fit_transform(X)

matriz_cov = np.cov(xNew.T)

eig_val, eig_vec = np.linalg.eig(matriz_cov)

X_proy = xNew.dot(eig_vec.T[0])
eje_dummy = np.zeros(y.shape[0])

ax = plt.gca()
ax.scatter(X_proy, eje_dummy, c=y, cmap="Paired")
plt.xlabel('PCA')
plt.ylabel('y')
plt.show()