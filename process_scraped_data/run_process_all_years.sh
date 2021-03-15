#!/usr/bin/env bash

#### this file is used to process all scraped articles
#### for the years of interest

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# process each year individually
TARGET_YRS=($( seq 2005 2020 ))
TARGET_TYPES=("news" "news-and-views" "news-feature" "toolbox" "technology-feature" "career-column" "career-feature")
TARGET_TYPES=("news" "news-and-views" "news-feature" "toolbox" "technology-feature")

for TARGET_YEAR in "${TARGET_YRS[@]}"
do
    for TARGET_TYPE in "${TARGET_TYPES[@]}"
    do
        sh ${DIR}/run_process_target_year.sh ${TARGET_YEAR} ${TARGET_TYPE}
    done
done

