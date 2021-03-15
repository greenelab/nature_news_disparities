#!/usr/bin/env bash

#### this file is used to run the all scrapes

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# get in the correct directory to run the spider
cd ${DIR}/nature_news_scraper

# now crawl over the years of interest
TARGET_YRS=($( seq 2005 2020  ))
TARGET_TYPES=("news" "news-and-views" "news-feature" "toolbox" "technology-feature" "career-column" "career-feature")


for TARGET_YEAR in "${TARGET_YRS[@]}"
do
    for TARGET_TYPE in "${TARGET_TYPES[@]}"
    do
        # run for the news articles
        JSON_OUT_FILE="${DIR}/../data/doi_data/downloads/links_crawled_${TARGET_YEAR}_${TARGET_TYPE}.json"

        if [[ -f "$JSON_OUT_FILE" ]]; then
            echo "$JSON_OUT_FILE already exists. Skipping $TARGET_YEAR $TARGET_TYPE"
            continue
        fi

        # crawl
        scrapy crawl doi_crawl -O ${JSON_OUT_FILE} \
        -a target_year=${TARGET_YEAR} -a target_type=${TARGET_TYPE}
    done


done

