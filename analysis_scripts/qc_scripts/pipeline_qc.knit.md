---
title: "pipeline_qc"
author: "Natalie Davidson"
date: "1/20/2021"
output: github_document
---


## Nature News Pipeline QC

This document is for QC checking for each step of the pipeline. 
Here, we like to visualize and check statistics that should be stable across each article across all years.
The data we will be working with are the following:

1) Pipeline step 1, download data: `./data/scraped_data/downloads/links_crawled_YEAR.json` 
1) Pipeline step 2, process data into coreNLP readable: `./data/scraped_data/coreNLP_input_YEAR/*` 
2) Pipeline step 3, coreNLP output: `./data/scraped_data/coreNLP_output_YEAR/*`
2) Pipeline step 4, processed coreNLP output: `./data/quote_table_raw_*.tsv` 
    and `./data/location_table_raw_*.tsv`


**All analysis shown below depends on the functions described in `/analysis_scripts/analysis_utils.R`**


## QC pipeline step 1: Check scrapy output

Make sure there are no empty files.
Count the number of files to make sure this is constant across all steps.
Count the number of words in each article to compare down the entire pipeline.

























