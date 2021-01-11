#!/usr/bin/env bash

#### this file is used to process all scraped articles
#### for the years of interest

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# process each year individually
TARGET_YRS=( 2010 2015 2020 )
for TARGET_YEAR in "${TARGET_YRS[@]}"
do
    sh ${DIR}/run_process_target_years.sh ${TARGET_YEAR}
done

