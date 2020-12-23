#!/usr/bin/env bash

#### this file is used to run the initial "gold standard" scrapes

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# get in the correct directory to run the spider
cd ${DIR}/nature_news_scraper

# setup result files
JSON_LINK_FILE="${DIR}/../benchmark_data/links.json"
JSON_OUT_FILE="${DIR}/../benchmark_data/links_crawled.json"

# get the random files by doing a precrawl of 2010, 2015, 2020
scrapy crawl precrawl -O ${JSON_LINK_FILE} && ( 
    # run the scrape on 10 random articles 
    scrapy crawl benchmark -a json_links=${JSON_LINK_FILE} -O ${JSON_OUT_FILE}
)
