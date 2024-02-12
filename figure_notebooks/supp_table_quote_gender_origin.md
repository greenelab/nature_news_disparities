Quote\_Gender\_Name\_Origin
================
Natalie Davidson
5/3/2021

## Overview

This notebook generates a supplemental figure to highlight the interesection of gender and name origin.

The **data** it uses to build the plots are here:

This document compares mentions across predicted gender and name origin.

The names mentioned data file is: `./data/author_data/all_mentioned_fullname_pred.tsv` The bg data file is: `./data/author_data/all_author_fullname_pred.tsv`

The three corpi are indexed by the `corpus` column:

1.  `news_quotes`: **foreground** est. name origin of Nature News quoted speaker

2.  `nature_last`: **background** est. name origin of last author of Nature articles.

The **setting + helper functions** to generate the plots are here:

1.  plotting related functions: `/utils/plotting_utils.R`

2.  reading + data processing related functions: `/utils/scraper_processing_utils.R` and `/analysis_scripts/analysis_utils.R`

3.  nautre research article and springer specific data processing functions: `/process_doi_data/springer_scripts/springer_scrape_utils.R`

## Read in the data

``` r
# read in raw quotes data for filtering
full_quote_df = NA
quote_files = list.files(file.path(proj_dir,"/data/scraped_data/", sep=""), full.names = T)
quote_files = grep("quote_table_raw_", quote_files, value=T)
for(quote_file in quote_files){
    
    quote_df = read_corenlp_quote_files(quote_file)
    quote_df$year = str_extract(quote_file, "[1-9][0-9]+") # curr_year
    quote_df$type = substring(basename(quote_file), 
                            22, nchar(basename(quote_file))-4)
    
    full_quote_df = rbind(full_quote_df, quote_df)
}
full_quote_df = full_quote_df[-1,]

# filter out articles with more than 25 quotes
num_quotes = table(full_quote_df$file_id)
too_many_quotes_idx = which(num_quotes > 25)
too_many_quotes_file_id = names(num_quotes)[too_many_quotes_idx]


# first read in the quote data
name_pred_file = file.path(proj_dir, 
                             "/data/author_data/all_speaker_fullname_pred.tsv")
name_info_file = file.path(proj_dir, 
                             "/data/author_data/all_speaker_fullname.tsv")

quote_name_df = read_name_origin(name_pred_file, name_info_file)
quote_name_df$name_origin[quote_name_df$name_origin == "Jewish"] = "Hebrew"
quote_name_df = subset(quote_name_df, !file_id %in% too_many_quotes_file_id)
quote_name_df = quote_name_df[which(quote_name_df$year <= 2020),]


# filter out career column and news-and-views
quote_name_df = subset(quote_name_df, !type %in% c("career-column", "news-and-views", "guardian"))
quote_name_df = unique(quote_name_df)
```

## Process Data

### get predicted gender estimate

``` r
gender_info_file = file.path(proj_dir, 
                             "data/reference_data/genderize.tsv")
gender_update_file = file.path(proj_dir, 
                             "data/reference_data/genderize_update.tsv")

gender_df = data.frame(fread(gender_info_file))
gender_update_df = data.frame(fread(gender_update_file))
gender_df = rbind(gender_df, gender_update_df)

name_df_split = separate(data = quote_name_df, col = author, into = c("fore_name_simple", "author_lastname"), sep = " ")
name_df_split$fore_name_simple = tolower(name_df_split$fore_name_simple)
name_origin_gender_df = merge(name_df_split, gender_df)

name_origin_gender_df = name_origin_gender_df[which(!is.na(name_origin_gender_df$probability_male)),]
name_origin_gender_df$predicted_gender = "MALE"
name_origin_gender_df$predicted_gender[name_origin_gender_df$probability_male < 0.5] = "FEMALE"
```

### Format table for results

``` r
gender_origin = table(name_origin_gender_df$name_origin, name_origin_gender_df$predicted_gender)
ratio_male = gender_origin[,2] / (gender_origin[,1] + gender_origin[,2])
gender_origin = cbind(gender_origin, ratio_male)
gender_origin
```

    ##               FEMALE  MALE ratio_male
    ## African          270  1554  0.8519737
    ## ArabTurkPers     346  1765  0.8360966
    ## CelticEnglish   6399 33329  0.8389297
    ## EastAsian       1090  4438  0.8028220
    ## European        4788 22844  0.8267226
    ## Greek             73   445  0.8590734
    ## Hebrew           213  1303  0.8594987
    ## Hispanic         760  2450  0.7632399
    ## Nordic           593  2397  0.8016722
    ## SouthAsian       465  2019  0.8128019

``` r
knitr::kable(gender_origin, format = "pipe", 
             caption = "Quoted speaker gender by name origin")
```

|               |  FEMALE|   MALE|  ratio\_male|
|:--------------|-------:|------:|------------:|
| African       |     270|   1554|    0.8519737|
| ArabTurkPers  |     346|   1765|    0.8360966|
| CelticEnglish |    6399|  33329|    0.8389297|
| EastAsian     |    1090|   4438|    0.8028220|
| European      |    4788|  22844|    0.8267226|
| Greek         |      73|    445|    0.8590734|
| Hebrew        |     213|   1303|    0.8594987|
| Hispanic      |     760|   2450|    0.7632399|
| Nordic        |     593|   2397|    0.8016722|
| SouthAsian    |     465|   2019|    0.8128019|
