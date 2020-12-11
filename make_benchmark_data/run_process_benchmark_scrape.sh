#!/usr/bin/env bash

## this file runs the initial processing after scraping
## to make the benchmark dataset


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## first process the json files from the scrape to make them readable 
## by Stanford CoreNLP
IN_FILE="${DIR}/../benchmark_data/links_crawled.json"
OUT_DIR="${DIR}/../benchmark_data/coreNLP_input/"
mkdir -p ${OUT_DIR}
RScript ${DIR}/process_benchmark_scrape.R ${IN_FILE} ${OUT_DIR}

## now run StanfordNLP on the benchmark files
CORENLP_OUTPUT="${DIR}/../benchmark_data/coreNLP_output/"
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
BENCHMARK_RAW_FILE="${DIR}/../benchmark_data/benchmark_quote_table_raw.tsv"
RScript ${DIR}/process_benchmark_corenlp_output.R ${CORENLP_OUTPUT} ${BENCHMARK_RAW_FILE}
