
from pybliometrics.scopus.utils import config
from pybliometrics.scopus import AbstractRetrieval
from pybliometrics.scopus import AuthorRetrieval

import sys, getopt
import json

import os
import pandas as pd

def read_json_files(in_dir):

    if in_dir == '':
        print("no input directory found, please pass this parameter to script")
        sys.exit()

    # for each non-empty entry in input json file
    # get author info
    full_doi_df = []
    for in_file in os.listdir(in_dir):
        full_path = os.path.join(in_dir, in_file)

        if os.path.getsize(full_path) == 0:
            continue

        data = pd.read_json(full_path)

        # selecting rows based on condition 
        doi_df = data[data['dois'] != '']

        # split on , in dois
        doi_df['dois'] = doi_df['dois'].str.split(', ')
        doi_df = doi_df.explode('dois')

        # strip out quotes and doi string
        doi_df['dois'] = doi_df['dois'].str.replace('\"', '')
        doi_df['dois'] = doi_df['dois'].str.replace('doi:', '')

        # add in the file name for reference
        doi_df['filename'] = in_file

        # store DataFrame in list
        full_doi_df.append(doi_df)

    # see pd.concat documentation for more info
    full_doi_df = pd.concat(full_doi_df)

    return full_doi_df


def get_author_info(api_key, full_doi_df, out_file):

    if api_key == '':
        print("no scopus API key found, please set this in setup.sh")
        sys.exit()

    if out_file == '':
        print("no scopus API key found, please pass this parameter to script")
        sys.exit()

    # set elsevier API key
    config['Authentication']['APIKey'] = api_key
    config['Authentication']['APIKey'] = 'ae93295cfed56f688ef61305d61b3634'
    config['Authentication']['InstToken'] = ''

    # now for each row in full_doi_df
    # get the author info: name, position, affiliation
    author_info_df = []
    for curr_row in full_doi_df.iterrows():
        curr_row = 
        ab = AbstractRetrieval(curr_row[['dois']])

curl -L -H 'X-ELS-APIKey: ae93295cfed56f688ef61305d61b3634' 'http://api.elsevier.com/authenticate?platform=SCOPUS'        
'aa526f60fdf49130aa089a4728046233'
curl -L 'http://api.springernature.com/meta/v2/json?q=doi:10.1038/s41467-020-18659-3&api_key=aa526f60fdf49130aa089a4728046233' | jq
    


def main(argv):
    api_key = ''
    in_dir = ''
    out_file = ''
    try:
        opts, args = getopt.getopt(argv,"hi:o:a:",["in_dir=","out_file=","apikey="])
    except getopt.GetoptError:
        print 'test.py -i <in_dir> -o <out_file> -a <apikey>'
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print 'process_news_dois.py -i <in_dir> -o <out_file> -a <apikey>'
            sys.exit()
        elif opt in ("-a", "--apikey"):
            api_key = arg
        elif opt in ("-i", "--in_dir"):
            in_dir = arg
        elif opt in ("-o", "--out_file"):
            out_file = arg

    print 'API key is "', api_key
    print 'Input directory is "', in_dir
    print 'Output file is "', out_file

    full_doi_df = read_json_files(in_dir)
    get_author_info(api_key, full_doi_df, out_file)

if __name__ == "__main__":
   main(sys.argv[1:])



