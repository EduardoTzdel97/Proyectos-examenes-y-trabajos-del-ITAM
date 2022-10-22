#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: ivan
"""

import matplotlib.pyplot as plt
from sklearn.cluster import DBSCAN
from sklearn.preprocessing import StandardScaler
from sklearn import datasets

# Datos de entrenamiento  
X, y = datasets.load_iris(return_X_y=True)
X = StandardScaler().fit_transform(X)

plt.scatter(X[:, 0], X[:, 1], c=y, cmap='Paired',s=50)
plt.show()

modelo = DBSCAN(eps=0.75, min_samples=10).fit(X)
y2     = modelo.labels_

plt.scatter(X[:, 0], X[:, 1], c=y2, cmap='Paired',s=50)
plt.show()
