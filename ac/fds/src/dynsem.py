# File:       dynsem.py
# Purpose:    Implement dynamic interpretation semantics for Word2Vec
# Institute:  Department of Philosophy, University of Amsterdam
# Author:     Mick de Neeve <mick@live.nl, https://deneeve.github.io/ac/edu/fds>
# Date:       December 13, 2022


from gensim.models import Word2Vec
import kldiv as kl
import fileio as io
import numpy as np


#def exp(hyp)


# uni(int D) = Distr U
#   Make uniform probability distribution to serve as ignorant initial state.
#
def uni(D):

    return kl.softmax(np.full(D, 1.0))
    #return np.full(D, 1.0)

# mul(Vec U, Vev V) = Distr M
#   Multiply two vectors (which may be probability distributions) into a
#   probability distribution: state update as conjunction under the
#   independence assumption.
#
def mul(U, V):

    return kl.softmax(np.array(U)*np.array(V))
    #return np.array(U)*np.array(V)

# add(Vec U, Vec V) = Distr A
#   Add two vectors into a probability distribution, dropping the
#   independence assumption, but instead assuming the words in question
#   represent the entire probability mass.
#
def add(U, V):

    return kl.softmax(np.array(U)+np.array(V))
    #return kl.softmax(kl.softmax(np.array(U))+kl.softmax(np.array(V)))
    #return np.array(U)+np.array(V)

# max(Vec U, Vev V) = Distr M
#   Implement coincidence of equal-sized events vectors as the maximum value
#   of the two, and return the result as a probability distribution.
#
def max(U, V):
    
    #M = []
    
    #for i in range(len(U)):
    #    M.append(max(U[i], V[i]))
    
    #return kl.softmax(np.array(M))
    
    return kl.softmax(np.maximum(np.array(U), np.array(V)))
    #return np.maximum(np.array(U), np.array(V))

