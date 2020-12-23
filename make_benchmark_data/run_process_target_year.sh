#!/usr/bin/env bash

## this file runs the initial processing after scraping
## to make the full dataset
TARGET_YEAR=$1


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


## first process the json files from the scrape to make them readable 
## by Stanford CoreNLP
IN_FILE="${DIR}/../scraped_data/downloads/links_crawled_${TARGET_YEAR}.json"
OUT_DIR="${DIR}/../scraped_data/coreNLP_input/"
mkdir -p ${OUT_DIR}
RScript ${DIR}/process_benchmark_scrape.R ${IN_FILE} ${OUT_DIR}

## now run StanfordNLP on the all files
CORENLP_OUTPUT="${DIR}/../scraped_data/coreNLP_output/"
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

## process the results
RAW_FILE="${DIR}/../scraped_data/quote_table_raw_${TARGET_YEAR}.tsv"
RScript ${DIR}/process_benchmark_quotes_corenlp_output.R ${CORENLP_OUTPUT} ${RAW_FILE}

RAW_FILE="${DIR}/../scraped_data/location_table_raw_${TARGET_YEAR}.tsv"
RScript ${DIR}/process_benchmark_locations_corenlp_output.R ${CORENLP_OUTPUT} ${RAW_FILE}
