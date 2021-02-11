location\_analysis
================
Natalie Davidson
1/22/2021

## Nature News Location Bias

This document is a working analysis of the quotes extracted from Nature News content to see if there are differences in gender representation. The data we will be working with are the following:

1.  `./data/benchmark_data/benchmark_quote_table_raw.tsv` is the output after scraping a randomly selected set of 10 articles from 2010, 2015, or 2020 (`./nature_news_scraper/run_scrape_benchmark.sh`) then running it through coreNLP with additional processing (`./process_scraped_data/run_process_target_year.sh`)
2.  `./data/scraped_data/quote_table_raw_20*.tsv` are the output after scraping all articles from a year between 2001 2020 (`./nature_news_scraper/run_scrape_benchmark.sh`) then running it through coreNLP with additional processing (`./process_scraped_data/run_process_target_year.sh`)

**All analysis shown below depends on the functions described in `/analysis_scripts/analysis_utils.R`**

## Check Benchmark Consistency

We would like to make sure that our benchmark data is representative of our complete dataset, so let's look at some quote stats to make sure.

### reading in the quote data

First we read in the benchmark data

``` r
# get the project directory, everything is set relative to this
proj_dir = here()

# read in the benchmark quote table
bm_loc_file = paste(proj_dir, 
                    "/data/benchmark_data/benchmark_location_table_raw.tsv", 
                    sep="")

bm_loc_df = read_corenlp_location_files(bm_loc_file)

# add the year annotation
year_idx_file = paste(proj_dir, 
                "/data/benchmark_data/coreNLP_input/fileID_year.tsv", 
                sep="")
year_df = data.frame(fread(year_idx_file))
bm_loc_df = merge(year_df, bm_loc_df)


head(bm_loc_df)
```

    ##         file_id year est_country_code
    ## 1 4641259a.html 2010               us
    ## 2 4641259a.html 2010               us
    ## 3 4641259a.html 2010               us
    ## 4 4641259a.html 2010               us
    ## 5 4641259a.html 2010               us
    ## 6 4641259a.html 2010               us
    ##                                               text               ner
    ## 1                                       new mexico STATE_OR_PROVINCE
    ## 2                                      connecticut STATE_OR_PROVINCE
    ## 3                                               ms STATE_OR_PROVINCE
    ## 4 national center for genome resources in santa fe      ORGANIZATION
    ## 5                                               us           COUNTRY
    ## 6                         university of california      ORGANIZATION
    ##     est_country est_un_region est_un_subregion
    ## 1 United States      Americas Northern America
    ## 2 United States      Americas Northern America
    ## 3 United States      Americas Northern America
    ## 4 United States      Americas Northern America
    ## 5 United States      Americas Northern America
    ## 6 United States      Americas Northern America

Now we read in the full data for the same years.

``` r
# read in the full year quote table for 2010, 2015-2020
full_loc_df = NA
for(curr_year in c(2005:2006, 2008, 2010:2020)){
    loc_file = paste(proj_dir, 
                    "/data/scraped_data/location_table_raw_", curr_year, ".tsv", 
                    sep="")
    loc_df = read_corenlp_location_files(loc_file)
    loc_df$year = curr_year

    full_loc_df = rbind(full_loc_df, loc_df)
}
full_loc_df = full_loc_df[-1,]

full_loc_df = subset(full_loc_df, est_un_region != "" & 
                                        est_un_subregion != "" &
                                        est_un_region != "NO_EST" & 
                                        est_un_subregion != "NO_EST")

head(full_loc_df)
```

    ##   est_country_code            file_id        text     ner est_country
    ## 2               af            437302a afghanistan COUNTRY Afghanistan
    ## 3               af     050228-17.html afghanistan COUNTRY Afghanistan
    ## 4               am     050221-17.html     armenia COUNTRY     Armenia
    ## 5               ao     050531-12.html      angola COUNTRY      Angola
    ## 6               ao  news050627-9.html      angola COUNTRY      Angola
    ## 7               ao news050328-11.html      angola COUNTRY      Angola
    ##   est_un_region est_un_subregion year
    ## 2          Asia    Southern Asia 2005
    ## 3          Asia    Southern Asia 2005
    ## 4          Asia     Western Asia 2005
    ## 5        Africa    Middle Africa 2005
    ## 6        Africa    Middle Africa 2005
    ## 7        Africa    Middle Africa 2005

Now we join the tables together for comparison.

``` r
full_loc_df$is_benchmark = "full_data"
bm_loc_df$is_benchmark = "benchmark"

bm_full_loc_df = rbind(subset(full_loc_df, year %in% c("2010", "2015", "2020")), 
                       bm_loc_df)
bm_full_loc_df = subset(bm_full_loc_df, est_un_region != "" & 
                                        est_un_subregion != "" &
                                        est_un_region != "NO_EST" & 
                                        est_un_subregion != "NO_EST")
```

### compare benchmark and non-benchmark data

Now lets check how well the benchmark proportions match the full dataset proportions. The number of articles per year in each benchmark set is 10, the total number of articles in the year is between 400-1000. Since the number of articles in the benchmark set is very small, we don't expect exact consistency between the benchmark and total data sets.

#### Lets look at the largest region definition first: UN region

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

#### Lets look at the UN Subregion level

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" /><img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-5-2.png" style="display: block; margin: auto;" />

#### Now Finally the Country Code level, except divided by subregion

    ## $`1`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

    ## 
    ## $`2`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-2.png" style="display: block; margin: auto;" />

    ## 
    ## $`3`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-3.png" style="display: block; margin: auto;" />

    ## 
    ## $`4`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-4.png" style="display: block; margin: auto;" />

    ## 
    ## $`5`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-5.png" style="display: block; margin: auto;" />

    ## 
    ## $`6`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-6.png" style="display: block; margin: auto;" />

    ## 
    ## $`7`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-7.png" style="display: block; margin: auto;" />

    ## 
    ## $`8`

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-6-8.png" style="display: block; margin: auto;" />

    ## 
    ## attr(,"class")
    ## [1] "list"      "ggarrange"

### Compare locations over all years

Now lets see the proportion of articles on the full dataset that mention each UN Subregion. This is shown in the next 2 plots: the top plot is all UN subregions and the bottom plot removes the Northern American and Northern Europe subregions in order to get a clearer look at the change in other subregions.

<img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display: block; margin: auto;" /><img src="location-analysis_all_years_files/figure-markdown_github/unnamed-chunk-7-2.png" style="display: block; margin: auto;" />
