#!/usr/bin/env bash

## this file runs the initial processing after scraping
## to make the full dataset
TARGET_YEAR=$1
TARGET_TYPE=$2


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## set up paths
IN_FILE="${DIR}/../data/scraped_data/downloads/links_crawled_${TARGET_YEAR}_${TARGET_TYPE}.json"
OUT_DIR="${DIR}/../data/scraped_data/coreNLP_input_${TARGET_YEAR}_${TARGET_TYPE}/"
CORENLP_OUTPUT="${DIR}/../data/scraped_data/coreNLP_output_${TARGET_YEAR}_${TARGET_TYPE}/"
CORENLP_INPUT=${OUT_DIR}/file_list.txt

# only run the coreNLP pipeline if the coreNLP results folder doesn't exist
if [ ! -d ${CORENLP_OUTPUT}  ]; then
    echo "running coreNLP pipeline for year ${TARGET_YEAR} ${TARGET_TYPE}"

    ## first process the json files from the scrape to make them readable 
    ## by Stanford CoreNLP
    mkdir -p ${OUT_DIR}
    RScript ${DIR}/process_scrape.R ${IN_FILE} ${OUT_DIR}

    ## now run StanfordNLP on the all files
    mkdir -p ${CORENLP_OUTPUT}


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

fi

## process the results, we do this always since this code is likely to change
#QUOTE_RES_FILE="${DIR}/../data/scraped_data/quote_table_raw_${TARGET_YEAR}_${TARGET_TYPE}.tsv"
#RScript ${DIR}/process_corenlp_quotes_corenlp_output.R ${CORENLP_OUTPUT} ${QUOTE_RES_FILE}

LOC_RES_FILE="${DIR}/../data/scraped_data/location_table_raw_${TARGET_YEAR}_${TARGET_TYPE}.tsv"
RScript ${DIR}/process_corenlp_locations_corenlp_output.R ${CORENLP_OUTPUT} ${LOC_RES_FILE}
