nature\_news\_disp
================
Natalie Davidson
12/16/2020

## Nature News Disparities -- gender + location

This document is a first attempt to analyze the Nature News content to see if there are differences in geographic and gender representation. Currently, this work only looks at a benchmark dataset with 10 articles per year, for the years 2010, 2015, 2020. The benchmark dataset currently consists of 2 files

1.  `benchmark_quote_table_hand_annotated` contains mapping between the speaker and their name and gender
    -   `benchmark_quote_table_raw` is the output from coreNLP which we will compare against
2.  `benchmark_location_table_hand_annotated` contains a mapping between all found (organizations, states, provencces, countries) with a (normalized country name, UN region, and UN sub-region)
    -   `benchmark_location_table_raw` is the output from coreNLP which we will compare against

**All analysis shown below depends on the functions described in `/analyze_benchmark_data/analyze_benchmark_data.R`**

## Quote Analysis

### reading in the quote data

``` r
# get the project directory
proj_dir = here()

# get benchmark (bm) file and read it
bm_quote_file = paste(proj_dir, 
                    "/data/benchmark_data/benchmark_quote_table_hand_annotated.tsv", 
                    sep="")

bm_quote_df = read_benchmark_quote_file(bm_quote_file)
```

Lets look at what the file

``` r
head(bm_quote_df)
```

    ##         file_id      true_speaker true_gender
    ## 1 4641259a.html  Sergio Baranzini        MALE
    ## 2 4641259a.html  Sergio Baranzini        MALE
    ## 3 4641259a.html Stephen Kingsmore        MALE
    ## 4 4641259a.html  Daniel Geschwind        MALE
    ## 5 4641259a.html Stephen Kingsmore        MALE
    ## 6 4641259a.html Stephen Kingsmore        MALE
    ##                                                                                                                                  quote
    ## 1                                                                 one was exposed to the perfect combination of environmental triggers
    ## 2 It isn't just sequence — they went from sequence to epigenome to expression. That's what really makes [the study] something special.
    ## 3                                                                           we really ought to look at sequencing of the brain tissue,
    ## 4                                                          What they've done here is create a very nice template for others to follow,
    ## 5                                                    Both twins came into the world with the same set of high risks for developing MS,
    ## 6                                                        There had to be some trigger that caused one to develop it and the other not,

Here we get the `file_id`, the true speaker of the quote, their true gender, and the quote in question. Now lets find what we get out of coreNLP, which we will compare against

``` r
# 
raw_quote_file = paste(proj_dir, 
                    "/data/benchmark_data/benchmark_quote_table_raw.tsv", 
                    sep="")

raw_quote_df = read_corenlp_quote_files(raw_quote_file)

head(raw_quote_df)
```

    ##              file_id        est_speaker est_gender  canonical_speaker
    ## 1 d41586-020-00889-6              Addex       MALE       spokesperson
    ## 2 d41586-020-00889-6              Addex       MALE        Neena Nizar
    ## 3 d41586-020-00889-6              Addex       MALE       spokesperson
    ## 4 news.2010.179.html Adrian de Ferranti       MALE Adrian de Ferranti
    ## 5 d41586-020-01756-0    Alexis Kalergis       MALE            Unknown
    ## 6 news.2010.179.html      Andrew Osmond       MALE            Unknown
    ##         partial_name
    ## 1              Addex
    ## 2              Addex
    ## 3              Addex
    ## 4 Adrian de Ferranti
    ## 5           Kalergis
    ## 6      Andrew Osmond
    ##                                                                                                                                                                                                                  quote
    ## 1                                                                                                                                                                                   The COVID-19 situation is dynamic,
    ## 2                                                                                                                        I feel like we were chugging along on a train and then somebody dropped a huge boulder on it.
    ## 3                                                                                                         We are now seeing impacts on clinical-trial continuity in all the regions where we conduct clinical studies.
    ## 4 It is an endorsement of the council and it's also a message to us that we've got a lot to do and we've got a terrific organization to bring into the twenty-first century and continue with the job we have in hand.
    ## 5                                                                                                                                              all human vaccines used in Chile are obtained from foreign laboratories
    ## 6                                                                                                                                                                  outgoings are now substantially in excess of income

The main columns of interest are `est_gender` and `est_speaker`, which we will compare between lines that have the same `quote` and `file_id`.

### analyzing quote data

First, lets look at the *benchmark* data, to see if there exist any gender disparity evidence. <img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Ok, so we see some signal. Now what does it look like for our estimated gender?

<img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

Nice, it looks pretty close. Things called `NO_EST` are when a quote was found, but no gender was able to be estimated. Let's take a closer look at the errors <img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### analyzing location data

``` r
# get benchmark (bm) file and read it
bm_loc_file = paste(proj_dir, 
                    "/data/benchmark_data/benchmark_location_table_hand_annotated.tsv", 
                    sep="")

bm_loc_df = read_benchmark_location_file(bm_loc_file)

raw_loc_file = paste(proj_dir, 
                    "/data/benchmark_data/benchmark_location_table_raw.tsv", 
                    sep="")

raw_loc_df = read_corenlp_location_files(raw_loc_file)
```

    ## Warning in fread(country_file): Detected 12 column names but the data has 13
    ## columns (i.e. invalid file). Added 1 extra default column name for the first
    ## column which is guessed to be row names or an index. Use setnames() afterwards
    ## if this guess is not correct, or fix the file write command that created the
    ## file to create a valid file.

The location data tries to find an organization, state, province, or country. After this it tries to tag it to a canonically named country, and UN defined regions. Let's take a look.

``` r
head(bm_loc_df)
```

    ##   true_country_code                                                    file_id
    ## 1                ar                                         d41586-020-01756-0
    ## 2                ar                                         d41586-020-01756-0
    ## 3                au climatologists-to-physicists-your-planet-needs-you-1.17270
    ## 4                au climatologists-to-physicists-your-planet-needs-you-1.17270
    ## 5                au                                         d41586-020-00166-6
    ## 6                au                                         d41586-020-00166-6
    ##                            text          ner true_country true_un_region
    ## 1                     Argentina      COUNTRY    Argentina       Americas
    ## 2             Sinergium Biotech ORGANIZATION    Argentina       Americas
    ## 3             Monash University ORGANIZATION    Australia        Oceania
    ## 4                     Australia      COUNTRY    Australia        Oceania
    ## 5 University of New South Wales ORGANIZATION    Australia        Oceania
    ## 6                     Australia      COUNTRY    Australia        Oceania
    ##           true_un_subregion
    ## 1             South America
    ## 2             South America
    ## 3 Australia and New Zealand
    ## 4 Australia and New Zealand
    ## 5 Australia and New Zealand
    ## 6 Australia and New Zealand

``` r
head(raw_loc_df)
```

    ##   est_country_code                                                   file_id
    ## 1               ao us-science-academies-take-on-human-genome-editing-1.17581
    ## 2               ar                                        d41586-020-01756-0
    ## 3               au                                        d41586-020-00166-6
    ## 4               au                                        d41586-020-02835-y
    ## 5               au                                        d41586-020-02835-y
    ## 6               au                                        d41586-020-02835-y
    ##                                                           text          ner
    ## 1                                                          nam ORGANIZATION
    ## 2                                                    argentina      COUNTRY
    ## 3                                                    australia      COUNTRY
    ## 4                                                        csiro ORGANIZATION
    ## 5 commonwealth scientific and industrial research organisation ORGANIZATION
    ## 6                                        james cook university ORGANIZATION
    ##   est_country est_un_region          est_un_subregion
    ## 1      Angola        Africa             Middle Africa
    ## 2   Argentina      Americas             South America
    ## 3   Australia       Oceania Australia and New Zealand
    ## 4   Australia       Oceania Australia and New Zealand
    ## 5   Australia       Oceania Australia and New Zealand
    ## 6   Australia       Oceania Australia and New Zealand

Similar to before we will match columns baed on their names, in `raw_loc_df` it has `est_` columns and in `bm_loc_df` is has matching `true_` columns

Now lets first look at the benchmark data ![](benchmark_analysis_files/figure-markdown_github/unnamed-chunk-9-1.png)![](benchmark_analysis_files/figure-markdown_github/unnamed-chunk-9-2.png)![](benchmark_analysis_files/figure-markdown_github/unnamed-chunk-9-3.png)

Ok, so we see a strong signal that US/Americas/Europe are mentioned at a much higher rate than other regions. We would like to also see this pattern in our predicted locations, but first we need to show that our estimations are accurate. Shown below are now analyses comparing our hand-annotated benchmark data against the fully-automated processed data. We would like to show that the true number of articles with a region mention, is highly correlated to the estimated number of articles from our full pipeline.

First let's take a look at the prediction errors for UN Subregions

<img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

We find that there exist errors in the prediction, but it is not completely off. We would like to verify that our hand annotation and pipeline results are at least strongly correlated.

<img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-11-1.png" width="50%" /><img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-11-2.png" width="50%" /> Let's look at if subregions is any better/worse:

<img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-12-1.png" width="50%" /><img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-12-2.png" width="50%" />

Now, finally large regions:

<img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-13-1.png" width="50%" /><img src="benchmark_analysis_files/figure-markdown_github/unnamed-chunk-13-2.png" width="50%" />
