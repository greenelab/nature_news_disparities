background\_gender\_analysis
================
Natalie Davidson
3/01/2021

## Data Description

This document compares two "foreground" datasets (gender of quoted authors and gender of cited authors) and compares it to two possible "background" datasets (random sampling of 36K Springer articles, and all nature articles)

Foreground files:

1.  `./data/author_data/cited_author_gender.tsv` has the gender for each first and last authors that were cited in any nature news article between 2005-2020. Only articles that are indexed by Springer are included in this analysis.

2.  `./data/scraped_data/quote_table_raw_20*.tsv` has all quotes with estimated gender for the speaker. It is generated after scraping all articles from a year between 2005-2020 (`./nature_news_scraper/run_scrape_benchmark.sh`) then running it through coreNLP with additional processing (`./process_scraped_data/run_process_target_year.sh`)

Background files:

1.  `./data/author_data/springer_author_gender.tsv` has the gender of first and last authors from a randomly selected 36K Springer articles from 2005-2020.

2.  `./data/scraped_data/nature_author_gender.tsv` has the gender of first and last authors from all Nature articles from 2005-2020.

## Foreground Gender Breakdown

Read in the quote and citation data from Nature News.

``` r
# get the project directory, everything is set relative to this
proj_dir = here()

# read in the cited author data
read_gender_files <- function(in_file){
    in_df = data.frame(fread(in_file))
    colnames(in_df)[which(colnames(in_df) == "guessed_gender")] = "est_gender"
    in_df = subset(in_df, !is.na(year))
    return(in_df)
}

cited_file = file.path(proj_dir, "/data/author_data/cited_author_gender.tsv")
cited_df = read_gender_files(cited_file)
head(cited_df)
```

    ##   author                            doi year author_pos
    ## 1    a k          doi:10.1038/nphys4240 2019       last
    ## 2  aaron        doi:10.1038/nature10921 2012      first
    ## 3  aaron        doi:10.1038/nature25760 2018      first
    ## 4  aaron     doi:10.1186/1744-8603-9-43 2019      first
    ## 5  aaron doi:10.1038/s41467-019-09848-w 2020      first
    ## 6  aaron doi:10.1038/s41467-019-13176-4 2020       last
    ##                                    file_id est_gender gender
    ## 1                       d41586-019-03702-1       MALE   MALE
    ## 2 influenza-five-questions-on-h5n1-1.10874       MALE   MALE
    ## 3                       d41586-018-02096-w       MALE   MALE
    ## 4                       d41586-019-00210-0       MALE   MALE
    ## 5                       d41586-020-02461-8       MALE   MALE
    ## 6                       d41586-020-00094-5       MALE   MALE

``` r
# read in the all quotes for all news articles and all years 
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

head(full_quote_df)
```

    ##   file_id       est_speaker est_gender canonical_speaker     partial_name
    ## 2 434970a Arnold Sommerfeld       MALE     Edward Teller       Sommerfeld
    ## 3 438567a      Arthur Smith       MALE   Arthur E. Smith  Arthur E. Smith
    ## 4 437634a  Bertrand Russell       MALE  Bertrand Russell Bertrand Russell
    ## 5 434029a             Boxma       MALE           Unknown            Boxma
    ## 6 438031a           Bradley       MALE     F. H. Bradley    F. H. Bradley
    ## 7 435748a            Brooks       MALE   C. E. P. Brooks  C. E. P. Brooks
    ##                                                                                          quote
    ## 2     as if I was born in Germany only by mistake, and only came to my true homeland at age 28
    ## 3                                                                                       tongue
    ## 4 If ever these evils are eradicated, his name should stand very high indeed among the heroes.
    ## 5                                                                            true missing link
    ## 6                                          Finding bad reasons for what we believe on instinct
    ## 7                                                                                 Fog and Soot
    ##   year           type
    ## 2 2005 news-and-views
    ## 3 2005 news-and-views
    ## 4 2005 news-and-views
    ## 5 2005 news-and-views
    ## 6 2005 news-and-views
    ## 7 2005 news-and-views

### compare proportions over all years

From the Nature News corpus, we predict the gender of quoted speakers and cited (first and last) authors. Now lets plot the trend of predicted proportion of Male speakers/authors over time <img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-2-1.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-2-2.png" width="50%" />

## Background Gender Breakdown

Now we read in the background data: random sampling of Springer articles and all Nature articles.

``` r
# get the project directory, everything is set relative to this
proj_dir = here()


# read in the springer author data
springer_file = file.path(proj_dir, "/data/author_data/springer_author_gender.tsv")
springer_df = read_gender_files(springer_file)
head(springer_df)
```

    ##     author                            doi year author_pos est_gender gender
    ## 1      a s  doi:10.1007/s10586-017-1181-0 2017      first       MALE   MALE
    ## 2       aa  doi:10.1007/s00059-013-3953-5 2013      first       MALE   MALE
    ## 3       aa  doi:10.1007/s10751-012-0610-y 2012       last       MALE   MALE
    ## 4    aadil  doi:10.1007/s12591-015-0244-z 2015      first       MALE   MALE
    ## 5 aadithya  doi:10.1007/s12663-013-0500-0 2013       last       MALE   MALE
    ## 6    aafke doi:10.1007/s00204-020-02953-6 2020      first       MALE   MALE

``` r
# read in the nature author data
nature_file = file.path(proj_dir, "/data/author_data/nature_author_gender.tsv")
nature_df = read_gender_files(nature_file)
head(nature_df)
```

    ##   author                            doi year author_pos            file_id
    ## 1 aakash doi:10.1038/s41586-020-03052-3 2020      first s41586-020-03052-3
    ## 2  aaron        doi:10.1038/nature13124 2014      first        nature13124
    ## 3  aaron        doi:10.1038/nature25760 2018      first        nature25760
    ## 4  aaron  doi:10.1038/s41586-020-2944-y 2020      first  s41586-020-2944-y
    ## 5  aaron        doi:10.1038/nature13790 2014      first        nature13790
    ## 6  aaron        doi:10.1038/nature23912 2017      first        nature23912
    ##   est_gender gender
    ## 1       MALE   MALE
    ## 2       MALE   MALE
    ## 3       MALE   MALE
    ## 4       MALE   MALE
    ## 5       MALE   MALE
    ## 6       MALE   MALE

### compare gender authorship proportions over all years

Now lets look at all author publication gender and plot the trend over time. We see that the Springer corpus has many more articles sampled from it than the Nature corpus. We also see that there is an increase in Springer articles from 2005-2020; this is caused by name format changes. In this analysis, an article is only considered if it has a name where the gender is able to be guessed. In 2005, more journals were shortening the name such that only initials were used, thus reducing the number of articles able to be analyzed.

Since the number of articles in Nature are small and the resulting proportion of male authors noisy, we will use the Springer background in the following comparison.

<img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-4-1.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-4-2.png" width="50%" />

## Compare Foreground and Background

Now we will compare the identified quotes and cited authors from Nature News and compare the proportions of male speakers/authors against the previously shown background set of Springer articles.

<img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-2.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-5-3.png" width="50%" />

Now breakdown the quotes into the different submagazines.

<img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-6-2.png" width="50%" /><img src="gender_with_bg_analysis_files/figure-markdown_github/unnamed-chunk-6-3.png" width="50%" />
