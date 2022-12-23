#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Pan Yang (panyangnlp@gmail.com)
# Adapted by: Mick de Neeve (in/out paths & training args, September 2021)
# Copyright 2017 @ Yu Zhen

import gensim
import logging
import multiprocessing
import os
import re
import sys

from pattern.en import tokenize
from time import time

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s',
                    level=logging.INFO)


def cleanhtml(raw_html):
    cleanr = re.compile('<.*?>')
    cleantext = re.sub(cleanr, ' ', raw_html)
    return cleantext


class MySentences(object):
    def __init__(self, dirname):
        self.dirname = dirname

    def __iter__(self):
        for root, dirs, files in os.walk(self.dirname):
            for filename in files:
                file_path = root + '/' + filename
                for line in open(file_path, encoding="utf8"):
                    sline = line.strip()
                    if sline == "":
                        continue
                    rline = cleanhtml(sline)
                    tokenized_line = ' '.join(tokenize(rline))
                    is_alpha_word_line = [word for word in
                                          tokenized_line.lower().split()
                                          if word.isalpha()]
                    yield is_alpha_word_line


if __name__ == '__main__':
    if len(sys.argv) != 6:
        print ("Use: python(3) train_word2vec_with_gensim.py corpusdir modeldir contextradius vecsize sg")
        exit()

    corpusdir = sys.argv[1]
    modeldir = sys.argv[2]
    contextradius = int(sys.argv[3])
    vecsize = int(sys.argv[4])
    skipgram = int(sys.argv[5])

    begin = time()

    sentences = MySentences(corpusdir)
    model = gensim.models.Word2Vec(sentences,
                                   vector_size=vecsize,
                                   window=contextradius,
                                   sg=skipgram,
                                   min_count=10,
                                   workers=multiprocessing.cpu_count())
    model.save(modeldir+"/word2vec_gensim")
    model.wv.save_word2vec_format(modeldir+"/word2vec_org",
                                  modeldir+"/vocabulary",
                                  binary=False)

    end = time()

print ("Total procesing time: %d seconds" % (end - begin))
