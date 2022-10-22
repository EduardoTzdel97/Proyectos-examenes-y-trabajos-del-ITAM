# -*- coding: utf-8 -*-
"""
Created on Mon Oct 12 09:06:51 2020

@author: edu
"""

from sklearn.preprocessing import StandardScaler
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn import neighbors
from sklearn.neighbors import NearestCentroid

X, y = datasets.load_wine(return_X_y=True)

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.2)

#scaling data
scaler=StandardScaler()
scaler.fit(X_train)
X_train =scaler.transform(X_train)
X_test = scaler.transform(X_test)

n_neighbors = 3

modelo = neighbors.KNeighborsClassifier(n_neighbors)
modelo.fit(X_train,y_train)
print(modelo.predict(X_test))
modelo.fit
help(neighbors)

modelo2 = NearestCentroid()
modelo.fit(X_train,y_train)
print(modelo.predict(X_test))