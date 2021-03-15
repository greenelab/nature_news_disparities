location\_with\_bg\_analysis
================
Natalie Davidson
3/2/2021

## Data Description

This document compares two "foreground" datasets (locations mentioned and locations of authors cited in nature news articles) and compares it to two possible "background" datasets (random sampling of 36K Springer articles, and all nature articles)

The source data file is: `./data/author_data/all_author_country.tsv`

The four corpi are indexed by the `corpus` column:

1.  `nature_news`: **foreground** country of a location mentioned in any Nature News article

2.  `news_citation`: **foreground** country of Nature News cited authors affiliation.

3.  `nature_articles`: **background** country of author affiliation from Nature articles.

4.  `springer`: **background** country of author affiliation from random subset of Springer articles.

The `num_entries` column denotes the number of articles with at least ONE author from a particular country The `address.country_code` column denotes the UN 2-digit country code

A reference data file is: `./data/author_data/total_num_articles_per_corpus.tsv` This file contains the number of articles per corpus for reference.

## Foreground Location Breakdown

Read in the country data from all sources.

``` r
# get the project directory, everything is set relative to this
proj_dir = here()

# read in the cited author data
country_file = file.path(proj_dir, "/data/author_data/all_author_country.tsv")
country_df = data.frame(fread(country_file))

# get country info
country_info = get_country_info()

# add in NA's before merging for any country-year pairs that are missing
# beautify this code later
country_vec = unique(country_info$address.country_code)
corpus_vec = unique(country_df$corpus)
year_vec = 2005:2020
for(curr_country in country_vec){
    for(curr_year in year_vec){
        for(curr_corpus in corpus_vec){
            sub_matr = subset(country_df, 
                                year == curr_year & 
                                address.country_code == curr_country &
                                corpus == curr_corpus)
            if(nrow(sub_matr) == 0){
                new_entry = data.frame(year = curr_year,
                                       address.country_code = curr_country,
                                       num_entries = 0,
                                       corpus = curr_corpus)
                
                country_df = rbind(country_df, new_entry)
            }
        }
    }
}
country_df = subset(country_df, address.country_code != "")
country_df = merge(country_df, country_info)
country_df = subset(country_df, country != "")

# make all NAs 0
country_df$num_entries[is.na(country_df$num_entries)] = 0
head(country_df)
```

    ##   address.country_code year num_entries          corpus country un_region
    ## 1                   ad 2017           0     nature_news Andorra    Europe
    ## 2                   ad 2005           0   news_citation Andorra    Europe
    ## 3                   ad 2018           0 nature_articles Andorra    Europe
    ## 4                   ad 2018           0   news_citation Andorra    Europe
    ## 5                   ad 2006           0     nature_news Andorra    Europe
    ## 6                   ad 2005           0     nature_news Andorra    Europe
    ##      un_subregion
    ## 1 Southern Europe
    ## 2 Southern Europe
    ## 3 Southern Europe
    ## 4 Southern Europe
    ## 5 Southern Europe
    ## 6 Southern Europe

``` r
# read in the reference data
num_art_file = file.path(proj_dir, "/data/author_data/total_num_articles_per_corpus.tsv")
num_art_df = data.frame(fread(num_art_file))

head(num_art_df)
```

    ##   year tot_articles   corpus
    ## 1 2005       166340 springer
    ## 2 2006       185832 springer
    ## 3 2007       202791 springer
    ## 4 2008       229455 springer
    ## 5 2009       225054 springer
    ## 6 2010       244137 springer

``` r
#' this is the main function we will be using for aggregations for each plot
#' it aggregates over the region of interest (aggr_col_id),
#' denoted by the appropriate column name
#' from in_df.
#' 
#' @param in_df, data frame to aggregate over
#' @param num_art_df, data frame with total number of articles per year, per corpus
#' @param curr_corpus, corpus of interest
#' @param aggr_col_id, the column to aggregate over. Must be in in_df.
#' @return aggregated data frame, the aggregates will be in the column num_(aggr_col_id)
get_aggr_region <- function(in_df, num_art_df, curr_corpus, aggr_col_id){
    
    curr_df = subset(in_df, corpus == curr_corpus)
    curr_art_df = subset(num_art_df, corpus == curr_corpus)
    
    colnames(curr_df)[colnames(curr_df) == aggr_col_id] = "aggr_col"
    
    country_prop_df = curr_df %>% 
                        group_by(year, aggr_col) %>% 
                        summarise(sum(num_entries)) 
    aggr_num_id = paste("num", aggr_col_id, sep="_")
    colnames(country_prop_df)[3] = aggr_num_id
    
    country_prop_df = merge(country_prop_df, curr_art_df)
    
    country_prop_df$prop = country_prop_df[,aggr_num_id] / country_prop_df$tot_articles
    
    # plotting labels
    country_prop_df$label = country_prop_df$aggr_col
    country_prop_df$label[country_prop_df$year != 2020] = ""
    
    colnames(country_prop_df)[colnames(country_prop_df) == "aggr_col"] = aggr_col_id

    return(country_prop_df)

}
```

### compare cited vs mentioned regions over all years

From the Nature News corpus, lets compare the countries of locations mentioned in Nature news articles against the countries of cited authors.

Here lets first look at the total number of articles considered (number of nature news articles per year, and the number of articles cited by Nature News and indexed by Springer)

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

Let's first compare different UN subregions to one another in the two cohorts.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-3-2.png" style="display: block; margin: auto;" />

Let's subset the UN subregions and remove Northern America, Northern Europe, Western Europe, and Eastern Asia.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-4-2.png" style="display: block; margin: auto;" />

Now lets look at the proportion of articles with atleast 1 country mention or atleast 1 authors' affiliate country cited by Nature News.

We first look at individual countries.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-2.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-3.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-4.png" width="50%" />

Now lets take the mention proportion - citation proportion for each country. This will help us understand if some countries are studied more or publish more, or its equal.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-6-2.png" style="display: block; margin: auto;" />

## Background location Breakdown

Now aggregate the background data: all Springer articles and all Nature articles.

Here lets first look at the total number of articles considered (number of nature articles per year, and the number of Springer articles)

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-7-1.png" width="50%" />

So Springer has many more articles than Nature. Let's look comparatively at different countries to check their frequencies. We see that Nature is very biased towards US/UK in comparison to springer. I believe springer has non-english journals, but needs to be checked.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-8-1.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-8-2.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-8-3.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-8-4.png" width="50%" />

Now lets compare nature news citations rate against Springer and Nature articles for a few countries. We see that the citation rate mostly tracks the Nature article rate.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-9-1.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-9-2.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-9-3.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-9-4.png" width="50%" />

Now lets take the Nature authorship - Nature News citation proportion for each country. This will help us understand if Nature News focuses more on research from a specific country.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-10-1.png" style="display: block; margin: auto;" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-10-2.png" style="display: block; margin: auto;" />

Now lets compare nature news mentions rate against Springer and Nature articles for a few countries.

<img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-11-1.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-11-2.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-11-3.png" width="50%" /><img src="location_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-11-4.png" width="50%" />
