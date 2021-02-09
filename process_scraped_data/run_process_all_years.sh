#!/usr/bin/env bash

#### this file is used to process all scraped articles
#### for the years of interest

## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# process each year individually
TARGET_YRS=( 2011 2012 2013 2014 )
for TARGET_YEAR in "${TARGET_YRS[@]}"
do
    sh ${DIR}/run_process_target_year.sh ${TARGET_YEAR}
done

