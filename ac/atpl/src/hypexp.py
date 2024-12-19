# File:       hypexp.py
# Purpose:    Run experiments to determine directionality of hypernymy pairs
# Institute:  Department of Philosophy, University of Amsterdam
# Author:     Mick de Neeve <mick@live.nl, https://deneeve.github.io/ac/edu/fds>
# Date:       November 14, 2022


from __future__ import division
from gensim.models import Word2Vec
from sys import exit
import kldiv as kl
import fileio as io
import dynsem as dy
import numpy as np
import math


# resmat(Str hypfile, Str modbasedir, Bool(Int) s, Lambda formula,
#        Str expdir, Str filebase)
#   Evaluate a hypernymy data file against all available Word2Vec models of a
#   given architecture (i.e. CBOW or Skip-gram), and write a 3x9 matrix,
#   where rows are vector sizes, and columns context widths.
#
def resmat(hypfile, modbase, formula, expdir, filebase):
    
    vec = ['v100', 'v200', 'v300']
    con = ['c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10', 'c11', 'c12']
    
    hyp = io.readtuples(hypfile, ' ')
    matpath = expdir+'/'+'.'.join((filebase,arc,'mat.res'))
    
    mat = []
    
    for v in vec:
            
        row = []
    
        for c in con:
                    
            outpath = expdir+'/'+'.'.join((filebase,arc,v,c,'div'))
            moddir = modbase+'/'+'/'.join((arc,v,c))
            
            (mod,voc) = loadw2v(moddir)
            print('Loaded '+moddir+' ...')
            res = hypeval(hyp, voc, mod, formula, outpath)
            
            row.append('%07.4f' % round(res, 4))
            
        mat.append(tuple(row))
    
    io.writetuples(mat, matpath, ' ')
    print('Wrote '+matpath)


# resrow(Str hypfile, Str modbase, Bool(Int) s, Int v, Int c, Str expdir,
#        Str filebase)
#   Single-row version of resmat (above), meant for fixed architecture (s),
#   and vector (v) plus context sizes (c), where the hypernymy file is
#   evaluated using a single model and a list of formulas that combine the
#   model's layers (no intermediary results files are written).
#   NB:  Unlikely to be used.
#
def resrow(hypfile, modbase, v, c, expdir, filebase):

    vec = 'v'+str(v)
    con = 'c'+str(c)
    frm = [lambda CTX,WRD:(CTX,WRD), lambda CTX,WRD:(WRD,CTX), \
           lambda CTX,WRD:(CTX+WRD,WRD-CTX), lambda CTX,WRD:(CTX+WRD,WRD), \
           lambda CTX,WRD:(CTX+WRD,CTX), lambda CTX,WRD:(CTX*WRD,CTX), \
           lambda CTX,WRD:(CTX/WRD,CTX)]
    
    hyp = io.readtuples(hypfile, ' ')
    rowpath = expdir+'/'+'.'.join((filebase,arc,vec,con,'row.res'))
    moddir = modbase+'/'+'/'.join((arc,vec,con))
    
    (mod,voc) = loadw2v(moddir)
    print('Loaded '+moddir+' ...')
    
    row = []
    r = 1
    
    for f in frm:
        
        res = hypeval(hyp, voc, mod, f, '')
        print('Evaluated formula '+str(r)+' ...')
        r += 1
            
        row.append('%07.4f' % round(res, 4))
    
    io.writetuples([tuple(row)], rowpath, ' ')
    print('Wrote '+rowpath)


# hypeval(Tuples hyp, Dict voc, Model mod, Bool(Int) s, Lambda formula,
#         Bool(Str) out)
# = List scoretuples
#   Wrapper to call function wordseval, and optionally output results file.
#   NB:   Minimal version (non file writing) with lambda formula, needs
#         preliminary calls to loaddata, loadvocab and loadmodel.
#   
def hypeval(hyp, voc, mod, formula, out):

    tuples = wordseval(hyp, voc, mod, formula)

    if bool(out):
        io.writetuples(tuples, out, ' ')

    return expscore(tuples)


def tripeval(trip, mod):

    T = []
    
    for (w1,w2,w3) in trip:
        T.append(tripupdsem(w1, w2, w3, mod))
    
    return T


# wordseval(Tuples hyp, Dict voc, Model mod, Bool(Int), Lambda formula)
# = List scoretuples
#   Evaluate hypernymy data against model, using formula to combine model's
#   input and output layers.
#   TODO: Properly specify usage; roughly, with mod and voc fully
#         instantiated compute entropy assuming formula is lambda function;
#         if formula is non-negative compute word self-information; and
#         otherwise compute update semantics in three possible ways.
#
def wordseval(hyp, voc, mod, formula):

    E = []
    
    for (R,O) in hyp:   # R is hypernym, O is hyponym

        #(R_ctx, R_wrd) = (mod.wv[voc[R]], mod.syn1neg[voc[R]])
        #(O_ctx, O_wrd) = (mod.wv[voc[O]], mod.syn1neg[voc[O]])
        #
        #(Rf_x, Rf_y) = formula(R_ctx, R_wrd)
        #(Of_x, Of_y) = formula(O_ctx, O_wrd)
        #
        #(Rd, Od) = diverges(Rf_x, Rf_y, Of_x, Of_y)
        
        if bool(mod) and bool(voc):
            (Rd, Od) = divergence(R, O, voc, mod, formula)
        elif formula >= 0:
            (Rd, Od) = selfinf(R, O, voc, formula)
        else:
            (Rd, Od) = updsem(R, O, mod, formula)
        
        E.append((R,O,Rd,Od))
        
    return E


# divergence(String w1, String w2, Dict voc, Model mod, Lambda formula)
# = (Float d1, Float d2)
#   Compute Kullback-Leibler divergences for pairs (Rx, Ry) and (Ox, Oy),
#   i.e. for conditional and prior probability approximations for hypernyms
#   and hyponyms, respectively.
#   NB: possibly rename due to use of divergence elsewhere
#
def divergence(w1, w2, voc, mod, formula):

    (w1_ctx, w1_wrd) = (mod.wv[voc[w1]], mod.syn1neg[voc[w1]])
    (w2_ctx, w2_wrd) = (mod.wv[voc[w2]], mod.syn1neg[voc[w2]])
    
    (w1_fx, w1_fy) = formula(w1_ctx, w1_wrd)
    (w2_fx, w2_fy) = formula(w2_ctx, w2_wrd)
    
    #(d1, d2) = diverges(w1_fx, w1_fy, w2_fx, w2_fy)
    d1 = kl.kldivent(w1_fx, w1_fy)
    d2 = kl.kldivent(w2_fx, w2_fy)

    return (d1, d2)


# diverges(Vector Rx, Vector Ry, Vector Ox, Vector Oy) = (Float Rd, Float Od)
#   Compute Kullback-Leibler divergences for pairs (Rx, Ry) and (Ox, Oy),
#   i.e. for conditional and prior probability approximations for hypernyms
#   and hyponyms, respectively.
#
#def diverges(Rx, Ry, Ox, Oy):
#
#    Rd = kl.kldivent(Rx, Ry)
#    Od = kl.kldivent(Ox, Oy)
#     
#    return (Rd, Od)


# loadw2v(Str moddir) = (Model mod, Dict voc)
#   Returns Word2Vec model and vocab index dictionary.
#
def loadw2v(moddir):

    mod = Word2Vec.load(moddir+'/word2vec_gensim')
    voc = io.readdict(moddir+'/vocabulary', 'index')

    return (mod, voc)


# loaddata(Str hyppath) = List hyptuples
#   Load space-seperated hypernymy data, format: [(hypernym,hyponym), ...].
#
def loaddata(hyppath):
    
    return io.readtuples(hyppath, ' ')


# expscore(List tuples) = Float score
#   Compute score for a single experiment, i.e. a list of tuples yielded by
#   a Word2Vec model, of the form (word1,word2,kldiv1,kldiv2).
#
def expscore(tuples):

    length = len(tuples)
    count = 0
        
    for t in tuples:
        
        if float(t[2]) < float(t[3]):
            count = count + 1
        
    return count/length * 100


# selfinf(String w1, String w2, Dict voc, Int tv)
#   Return self-information pair for two words, i.e. their negative
#   log likelihoods based on corpus frequencies.
#
def selfinf(w1, w2, voc, tv):

    if not bool(tv) or callable(tv):
        tv = totvoc(voc)

    pw1 = voc[w1]/tv
    pw2 = voc[w2]/tv
    
    return (-math.log(pw1), -math.log(pw2))


# updsem(Str w1, Str w2, Model mod, Int upd)
#    Update initial ignorant state in two ways: (1) hyponym w2 first and then
#    hypernym w1; (2) vice versa, and measure the entropy difference, with
#    variable upd specifying update methods: -1 multiplication, -2 addition,
#    and -3 the max function.
#    TODO: Initial state must be specified with model dimensionality!
#
def updsem(w1, w2, mod, upd):

    state0 = dy.uni(300)
    
    vec1 = mod.wv[w1]
    vec2 = mod.wv[w2]
    
    state1a = dy.mul(state0,vec2) if upd==-1 else dy.add(state0,vec2) if upd==-2 \
              else dy.max(state0,vec2)
    state2a = dy.mul(state1a,vec1) if upd==-1 else dy.add(state1a,vec1) if upd==-2 \
              else dy.max(state1a,vec1)

    state1b = dy.mul(state0,vec1) if upd==-1 else dy.add(state0,vec1) if upd==-2 \
              else dy.max(state0,vec1)
    state2b = dy.mul(state1b,vec2) if upd==-1 else dy.add(state1b,vec2) if upd==-2 \
              else dy.max(state1b,vec2)

    da = kl.kldivent(state1a, state2a)
    db = kl.kldivent(state1b, state2b)

    return (da, db)


def tripupdsem(w1, w2, w3, mod):

    state0 = dy.uni(300)
    
    vec1 = mod.wv[w1]
    vec2 = mod.wv[w2]
    vec3 = mod.wv[w3]
    
    state1a = dy.add(state0, vec3)
    state2a = dy.add(state1a, vec2)
    state3a = dy.add(state2a, vec1)

    state1b = dy.add(state0, vec1)
    state2b = dy.add(state1b, vec2)
    state3b = dy.add(state2b, vec3)

    res = 1 if kl.kldivent(state1a,state2a) < kl.kldivent(state2b,state3b) \
            and kl.kldivent(state2a,state3a) < kl.kldivent(state1b,state2b) \
            and kl.kldivent(state1a,state3a) < kl.kldivent(state1b,state3b) \
            else 0

    return (w1, w2, w3, res)

    #da = kl.kldivent(state1a, state2a)
    #db = kl.kldivent(state1b, state2b)

    #return (da, db)


# totvoc(Dict voc)
#   Get the total number of words in the corpus given by voc.
#
def totvoc(voc):

    return sum(list(voc.values()))


# resdif(Tuples tup1, Tuples tup2) = Tuples D
#   Return tuples (w1,w2,s1,s2) correctly classified in tup1 but
#   misclassified in tup2 (i.e. where s1 < s2).
#
def resdif(tup1, tup2):

    D = []
    length = len(tup1)
    
    for i in range(length):
    
        t1 = tup1[i]
        t2 = tup2[i]
        
        if float(t1[2]) < float(t1[3]) and not float(t2[2]) < float(t2[3]):
            D.append(t1)        

    return D

