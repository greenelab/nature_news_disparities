#!/usr/bin/env bash

#### this file is used to process all scraped articles
#### for the years of interest

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# process each year individually
TARGET_YRS=($( seq 2020 2023 ))
TARGET_TYPES=("guardian")
TARGET_TYPES=("news" "news-and-views" "news-feature" "toolbox" "technology-feature" "career-column" "career-feature")


for TARGET_YEAR in "${TARGET_YRS[@]}"
do
    for TARGET_TYPE in "${TARGET_TYPES[@]}"
    do
        sh ${DIR}/run_process_target_year.sh ${TARGET_YEAR} ${TARGET_TYPE}
        echo "--------------------------------------------------"
        echo $TARGET_YEAR
        echo $TARGET_TYPE
        echo "--------------------------------------------------"
    done
done

FREQ_RES_FILE="${DIR}/../data/scraped_data/freq_table_raw.tsv"
RScript ${DIR}/process_corenlp_freq_corenlp_output.R ${FREQ_RES_FILE}
