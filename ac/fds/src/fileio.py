# File:       fileio.py
# Purpose:    Provide file reading and writing routines for Word2Vec
#             hypernymy experiments
# Institute:  Department of Philosophy, University of Amsterdam
# Author:     Mick de Neeve <mick@live.nl, https://deneeve.github.io/ac/edu/fds>
# Date:       May 31, 2022


# readtuples(Str filepath, Str separator) = List T
#   Read a list of tuples from file, such that each input line is a tuple.
#
def readtuples(path, sep):

    if sep == '':
        with open(path) as F:
            T = [tuple(line.strip().split()) for line in F]
    else:
        with open(path) as F:
            T = [tuple(line.strip().split(sep)) for line in F]

    return T


# writetuples(List tuples, Str path, Str separator)
#   Write a list of tuples to file, each tuple to a new line.
#
def writetuples(tuples, path, sep):

    F = open(path, 'w')
    
    for t in tuples:
        F.write(sep.join(str(s) for s in t) + '\n')

    F.close()


# writeprolog(List tuples, Str path, Str functor)
#   Version of writetuples for writing Prolog facts.
#
def writeprolog(tuples, path, functor):

    F = open(path, 'w')
    
    for t in tuples:
        F.write(functor + '(' + ','.join(str(s) for s in t) + ').\n')
    
    F.close()


# readarray2d(String path) = Array A
#   Read and return a matrix of floats.
#
def readarray2d(path):

    with open(path) as F:
        A = [[float(num) for num in line.split()] for line in F]

    return A


# readdict(Str path, Str mode)
#   Returns (vocabulary) dictionary with vocab items as keys and values
#   depending on mode, which can be 'index' in which case the running counter
#   is used, or 'value', meaning occurence nr next to the word is returned,
#   or 'zero' (fallback default) which sets all vallues to 0 for counting.
#
def readdict(path, mode):

    D = {}
    index = 0
    
    with open(path, encoding="utf8") as F:
    
        for line in F:
            
            (key,value) = line.split()
            D[key] = dictmode(index, int(value), mode)
            
            index = index + 1

    return D


# dictmode(Int index, Int value, Str mode)
#   Decide dictionary values based on mode ('index', 'value', or 'zero').
#
def dictmode(index, value, mode):

    if mode == 'index':
        return index
    elif mode == 'value':
        return value
    else:
        return 0
