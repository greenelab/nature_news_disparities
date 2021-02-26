#!/usr/bin/env bash

#### this file is used to run the all scrapes

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# get in the correct directory to run the spider
cd ${DIR}/nature_news_scraper

# now crawl over the years of interest
TARGET_YRS=($( seq 2005 2020  ))


for TARGET_YEAR in "${TARGET_YRS[@]}"
do

    # run for the news articles
    JSON_OUT_FILE="${DIR}/../data/author_data/downloads/links_crawled_${TARGET_YEAR}.json"

    if [[ -f "$JSON_OUT_FILE" ]]; then
        echo "$JSON_OUT_FILE already exists. Skipping $TARGET_YEAR"
        continue
    fi

    # crawl
    scrapy crawl author_crawl -O ${JSON_OUT_FILE} \
    -a target_year=${TARGET_YEAR}

done

