#!/usr/bin/env bash

#### this file is used to run the initial "gold standard" scrapes

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# get in the correct directory to run the spider
cd ${DIR}/nature_news_scraper

# now crawl over the years of interest
TARGET_YRS=( 2010 2015 2020 )
for TARGET_YEAR in "${TARGET_YRS[@]}"
do
	JSON_OUT_FILE="${DIR}/../scraped_data/downloads/links_crawled_${TARGET_YEAR}.json"

    # crawl
    scrapy crawl target_year_crawl -O ${JSON_OUT_FILE} \
    -a target_year=${TARGET_YEAR}

done

