#!/usr/bin/env bash

## this file runs the initial processing after scraping
## to make the benchmark dataset


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## first process the json files from the scrape to make them readable 
## by Stanford CoreNLP
GOLD_STANDARD_FILE="${DIR}/../data/benchmark_data/benchmark_quote_table_hand_annotated.tsv"
CORENLP_OUTPUT="${DIR}/../data/benchmark_data/benchmark_quote_table_raw.tsv"


RScript ${DIR}/process_benchmark_scrape.R ${GOLD_STANDARD_FILE} ${CORENLP_OUTPUT}
