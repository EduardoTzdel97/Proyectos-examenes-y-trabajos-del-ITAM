# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 16:36:33 2020

@author: Edu
"""

##############################################

import matplotlib.pyplot as plt
from sklearn.cluster import AgglomerativeClustering
from sklearn.datasets.samples_generator import make_blobs

 


# Datos de entrenamiento  
X,Y = make_blobs(centers=4,random_state=123,n_samples=1000)
plt.scatter(X[:, 0], X[:, 1], c='green',s=50)
plt.show()

 

clustering = AgglomerativeClustering(n_clusters=3)
clustering.fit(X)
y2 = clustering.labels_

 

plt.scatter(X[:, 0], X[:, 1], c=y2, cmap='Paired',s=50)
plt.show()



#################################
import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.cluster import AgglomerativeClustering

 

X, y = datasets.load_iris(return_X_y=True)

 


plt.scatter(X[:, 0], X[:, 2], c=y, cmap='Paired',s=50)
plt.show()

 


clustering = AgglomerativeClustering(n_clusters=5)
clustering.fit(X)
y2 = clustering.labels_

 

plt.scatter(X[:, 0], X[:, 2], c=y2, cmap='Paired',s=50)
plt.show()


import numpy as np

from matplotlib import pyplot as plt
from scipy.cluster.hierarchy import dendrogram
from sklearn import datasets
from sklearn.cluster import AgglomerativeClustering


def plot_dendrogram(model, **kwargs):

    # Children of hierarchical clustering
    children = model.children_

    # Distances between each pair of children
    # Since we don't have this information, we can use a uniform one for plotting
    distance = np.arange(children.shape[0])

    # The number of observations contained in each cluster level
    no_of_observations = np.arange(2, children.shape[0]+2)

    # Create linkage matrix and then plot the dendrogram
    linkage_matrix = np.column_stack([children, distance, no_of_observations]).astype(float)

    # Plot the corresponding dendrogram
    dendrogram(linkage_matrix, **kwargs)
    
    
X, y = datasets.load_iris(return_X_y=True)
x = X[:20]

modelo = AgglomerativeClustering()
modelo.fit(x)
plot_dendrogram(modelo)

plt.show()
