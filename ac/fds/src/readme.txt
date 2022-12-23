After downloading the corpus, use WikiExtractor (in Python 2) by issuing

    python WikiExtractor.py -o CORPUSDIR enwiki-20210920-pages-articles.xml

This creates a range of subdirectories AA/ to EN/, each of these with files named wiki_00 to wiki_99 of 1MB each.

Model training is done using the Python script train_word2vec_with_gensim.py, for instance, to train a Skip-gram model of 300 dimensions and context range 6, issue the following on the command line (in Python 3, this and subsequent; expect 15-20h):

    python train_word2vec_with_gensim.py CORPUSDIR MODELDIR 6 300 1

To load a model:

    import hypexp as hp
    (mod,voc) = hp.loadw2v('MODELDIR')

To load a hypernymy file (e.g. Baroni's EACL dataset):

    hyp = hp.loaddata('DATADIR')

To evaluate the model using information gain and write word pair divergences to FILE:

    hp.hypeval(hyp, voc, mod, lambda CTX,WRD:(CTX+WRD,CTX), 'FILE')
    
The output is a precision percentage.

To evaluate using self-information:

    import fileio as io
    valvoc = io.readdict('MODELDIR', 'value')
    hp.hypeval(hyp, valvoc, 0, hp.totvoc(valvoc), 'FILE')

To evaluate using update semantics (using addition for state update):

    hp.hypeval(hyp, 0, mod, -2, 'FILE')

In all three cases, if '' is passed instead of 'FILE', no file is written.
