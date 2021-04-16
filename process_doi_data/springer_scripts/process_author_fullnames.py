
# this file is a mix of code taken from 2 main files:
# https://github.com/greenelab/wiki-nationality-estimate/blob/master/utils.py
# https://github.com/greenelab/wiki-nationality-estimate/blob/master/07.test-ismb-data.py


import sys
import pickle
import argparse
import requests
import numpy as np
import pandas as pd
from keras.models import load_model
from keras.preprocessing import sequence

def find_ngrams(text, n, idx_dic):
    # https://github.com/greenelab/wiki-nationality-estimate/blob/master/utils.py
    a = zip(*[text[i:] for i in range(n)])
    wi = []
    for i in a:
        w = ''.join(i)
        try:
            idx = float(idx_dic[w])
        except KeyError:
            idx = 0.
        wi.append(idx)
    return wi



def featurize_data(names_list, ngram, index_dic):
    # https://github.com/greenelab/wiki-nationality-estimate/blob/master/utils.py
    feat_list = []
    for full_name in names_list:
        feats = find_ngrams(full_name, ngram, index_dic)
        feat_list.append(np.array(feats))
    return feat_list




# the following code is augmented from
# https://github.com/greenelab/wiki-nationality-estimate/blob/master/07.test-ismb-data.py

parser = argparse.ArgumentParser()
parser.add_argument("-l", "--ngrams", type=int, default=3)
parser.add_argument("-m", "--model_path", default="./name_lstm_models")
parser.add_argument("-n", "--names_file", default="./data/author_data/all_author_fullname.tsv")
parser.add_argument("-o", "--outfile", default="./data/author_data/all_author_fullname_pred.tsv")

args = parser.parse_args()

# Load model
model = load_model("%s/NamePrism.h5" % args.model_path)
filename = args.model_path + "/NamePrism_idx_dic.pkl"
idx_dic = pickle.load(open(filename, "rb"))
categories = pd.read_csv("%s/NamePrism_categories.txt" % args.model_path, header=None)

# Get PubMed author data from iscb-diversity repo
names_df = pd.read_csv(args.names_file, sep='\t')
pubmed_names_list = list(names_df['author'].drop_duplicates())

X_pubmed = featurize_data(pubmed_names_list, args.ngrams, idx_dic)
X_pubmed = sequence.pad_sequences(X_pubmed, maxlen=20)
y_pubmed_pred = model.predict_proba(X_pubmed, verbose=2)
print("PubMed authors",np.mean(y_pubmed_pred, axis=0))
y_pubmed_prob = pd.DataFrame(y_pubmed_pred, columns=categories[0], index=pubmed_names_list)
y_pubmed_prob.to_csv(args.outfile, sep='\t')

