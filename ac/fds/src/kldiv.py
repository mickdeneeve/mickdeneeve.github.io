# File:       kldiv.py
# Purpose:    Run kullback-leibler experiments on Word2Vec data
# Institute:  Department of Philosophy, University of Amsterdam
# Author:     Mick de Neeve <mick@live.nl, https://deneeve.github.io/ac/edu/fds>
# Date:       January 7, 2022


from __future__ import division
import numpy as np
import math
import scipy


# kldiv(Vector P, Vector Q) = Float div
#   Returns Kullback-Leibler divergence for vectors as distributions.
#
def kldivent(P, Q):

        return scipy.stats.entropy(softmax(P), softmax(Q))


# softmax(Array A) = Array M
#   Normalise A to return probability distribution M.
#
def softmax(A):

    E = np.exp(A - np.max(A))
    M = E/E.sum()
    
    return M
