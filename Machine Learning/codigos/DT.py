# -*- coding: utf-8 -*-
"""


@author: Edu
"""

from sklearn.preprocessing import StandardScaler
from sklearn import datasets
from sklearn import tree
from sklearn.model_selection import train_test_split

X, y = datasets.load_wine(return_X_y=True)

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.2)

#scaling data
scaler=StandardScaler()
scaler.fit(X_train)
X_train =scaler.transform(X_train)
X_test = scaler.transform(X_test)


modelo = tree.DecisionTreeClassifier()

modelo.fit(X_train,y_train)

tree.export_graphviz(modelo, out_file='arbol_vino.dot')