#!/usr/bin/env bash

## this file runs the initial processing after scraping
## to make the full dataset
TARGET_YEAR=$1


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


## first process the json files from the scrape to make them readable 
## by Stanford CoreNLP
IN_FILE="${DIR}/../data/scraped_data/downloads/links_crawled_${TARGET_YEAR}.json"
OUT_DIR="${DIR}/../data/scraped_data/coreNLP_input_${TARGET_YEAR}/"
mkdir -p ${OUT_DIR}
RScript ${DIR}/process_scrape.R ${IN_FILE} ${OUT_DIR}

## now run StanfordNLP on the all files
CORENLP_OUTPUT="${DIR}/../data/scraped_data/coreNLP_output_${TARGET_YEAR}/"
mkdir -p ${CORENLP_OUTPUT}

CORENLP_INPUT=${OUT_DIR}/file_list.txt

## check if the CLASSPATH is set
if [[ "$CLASSPATH" != *"stanford-corenlp-${CORENLP_VERSION}"* ]]; then
    echo "Classpath is not set, please set it or run 'setup.sh'"
fi

## now run coreNLP
java -Xmx5g edu.stanford.nlp.pipeline.StanfordCoreNLP \
    -fileList  ${CORENLP_INPUT} \
    -coref.algorithm statistical \
    -annotators tokenize,ssplit,pos,lemma,ner,parse,coref,quote \
    -outputDirectory ${CORENLP_OUTPUT} \
    -outputFormat json

