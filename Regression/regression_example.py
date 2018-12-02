import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.kernel_ridge import KernelRidge

""" Simple example of a ridge regression with an rbf kernel.
Data is generated here. 
"""

def getPowOfX(X, degree):
    xSeq = np.ones(shape = (len(X), degree))
    for i in range(xSeq.shape[1]):
        if i != 0:
            xSeq[:,i] = np.power(X, i)
    return xSeq

def Generate2DSamples(n, constants, xMin, xMax, var = 1, seed = 123):
    np.random.seed(seed)
    X = (xMax - xMin) * np.random.random(n) + xMin
    xSeq = getPowOfX(X, len(constants))
    ySeq = constants.dot(xSeq.T)

    Y = np.random.multivariate_normal(ySeq, np.diag(np.repeat(var, len(ySeq))))

    return X, Y


# Constants for the polynomial
constants = np.array([.6, -.2, -1.2, .2])
xMin, xMax = -5, 5
n = 500
var = 50
X, Y = Generate2DSamples(n, constants, xMin, xMax, var)

clf = KernelRidge(alpha=1.0, kernel = 'rbf')
X = X.reshape(-1,1)
Y = Y.reshape(-1,1)
clf.fit(X, Y)
x = np.arange(xMin, xMax, 0.01)
x = np.reshape(x, (-1,1))
y = clf.predict(x)
print(clf.score(X,Y))
plt.plot(X, Y, 'ro')
plt.plot(x,y,'g')
plt.show()
