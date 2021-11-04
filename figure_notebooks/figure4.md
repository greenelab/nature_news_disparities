Figure4\_citation\_v\_mention
================
Natalie Davidson
5/4/2021

## Overview

This notebook generates figure 4 and additional supplemental figures.

This analysis performs 3 main analyses

1.  looking at the citation rates of authors by country

2.  analyzing if the country citation rate is significantly different than the rate at which it is talked about

3.  for countries that have a very different citation vs mention rate, are there tokens that differentiate these countries?

For analysis 3, its main point is to identify how countries are talked about differently. To do this, we seperate countries into two groups: countries that are talked about vs cited. Specifically, we first identify which countries are cited more than mentioned and which countries are mentioned more than cited. After this, we will take the most exemplary of the 2 country classes (top mentions &gt; cited: Class M & top mentions &lt; cited: Class C). We will compare the token frequencies between a mention of Class C v M.

The **data** it uses to build the plots are here:

For analysis 1 the source data file is: `./data/author_data/all_author_country.tsv`

The four corpi are indexed by the `corpus` column:

1.  `nature_news`: **foreground** country of a location mentioned in any Nature News article

2.  `news_citation`: **foreground** country of Nature News cited authors affiliation.

3.  `nature_articles`: **background** country of author affiliation from Nature articles.

4.  `springer`: **background** country of author affiliation from random subset of Springer articles.

The `num_entries` column denotes the number of articles with at least ONE author from a particular country The `address.country_code` column denotes the UN 2-digit country code

For analysis 2+3 the source data files are:

1.  Bootstrap estimate of country mentions and citations: `/data/author_data/all_author_country_95CI.tsv`

2.  Source text for token analysis is here: `/data/scraped_data/downloads/*.json`

3.  Country mention to source articles id map here: `/data/scraped_data/location_table_raw_YEAR_ARTICLE-TYPE.tsv`

The **pdfs** included in the plots are here:

1.  `/figure_notebooks/illustrator_pdfs/`

The **setting + helper functions** to generate the plots are here:

1.  plotting related functions: `/utils/plotting_utils.R`

2.  reading + data processing related functions: `/utils/scraper_processing_utils.R` and `/analysis_scripts/analysis_utils.R`

3.  nautre research article and springer specific data processing functions: `/process_doi_data/springer_scripts/springer_scrape_utils.R`

## Read in the data

``` r
# read in the scraped news articles for each year
# we will need this later for filtering out articles in columns
# we would like to ignore
news_scraped_dir = file.path(proj_dir,
                    "/data/scraped_data/")
news_scraped_dir_files = list.dirs(news_scraped_dir, full.names = T)
news_scraped_dir_files = grep("coreNLP_output", news_scraped_dir_files, value=T)

news_df = NA
for(curr_dir in news_scraped_dir_files){
    
    curr_files = list.files(curr_dir, full.names = T)

    
    # if the json file was empty, skip
    if(length(curr_files) == 0 ){
        next
    }
    
    # get the year form the file name
    file_name_year = substring(basename(curr_dir), 
                            16, 19)
    
    # get the news article type from the file name
    file_name_type = substring(basename(curr_dir), 
                            21, nchar(basename(curr_dir)))
    
    # format the output
    article_ids = gsub(".txt.json", "", basename(curr_files))
    num_articles = length(article_ids)
    curr_info_df = data.frame(year=file_name_year, 
                                type=file_name_type, 
                                file_id=article_ids)
    news_df = rbind(news_df, curr_info_df)
    
}
news_df = news_df[-1,]

# filter out career column and news-and-views
news_df = subset(news_df, !type %in% c("career-column", "news-and-views"))
head(news_df)
```

    ##   year     type                                               file_id
    ## 2 2005 guardian                          missed_generize_io_names.tsv
    ## 3 2005 guardian news.2005.apr.28.thisweekssciencequestions.psychology
    ## 4 2005 guardian                 news.2005.dec.06.topstories3.genetics
    ## 5 2005 guardian                       news.2005.dec.21.food.christmas
    ## 6 2005 guardian        news.2005.feb.05.guardianobituaries.obituaries
    ## 7 2005 guardian            news.2005.feb.10.thisweekssciencequestions

``` r
#### read in the cited author data
country_file = file.path(proj_dir, "/data/author_data/all_author_country.tsv")
country_df = data.frame(fread(country_file))

# filter to only the articles we care about
file_id_keep = news_df$file_id
bg_df = subset(country_df, corpus %in% c("nature_articles", "springer_articles"))
to_filter_df = subset(country_df, corpus %in% c("naturenews_citations", "naturenews_mentions"))
to_filter_df = subset(to_filter_df, file_id %in% file_id_keep)
country_df = rbind(bg_df, to_filter_df)

# get UN info
un_info = get_country_info()
country_df = merge(country_df, un_info)
head(country_df)
```

    ##   address.country_code            file_id                            doi year
    ## 1                   ad            446937a            doi:10.1038/446937a 2007
    ## 2                   ae               <NA>    doi:10.1186/1752-1947-2-340 2008
    ## 3                   ae               <NA>  doi:10.1186/s12245-016-0104-9 2016
    ## 4                   ae       461462a.html       doi:10.1038/461462a.html 2009
    ## 5                   ae d41586-019-00807-5 doi:10.1038/d41586-019-00807-5 2019
    ## 6                   ae d41586-020-02244-1 doi:10.1038/d41586-020-02244-1 2020
    ##                corpus              country un_region    un_subregion
    ## 1 naturenews_mentions              Andorra    Europe Southern Europe
    ## 2   springer_articles United Arab Emirates      Asia    Western Asia
    ## 3   springer_articles United Arab Emirates      Asia    Western Asia
    ## 4 naturenews_mentions United Arab Emirates      Asia    Western Asia
    ## 5 naturenews_mentions United Arab Emirates      Asia    Western Asia
    ## 6 naturenews_mentions United Arab Emirates      Asia    Western Asia

``` r
#### read in the bootstrapped author data
ci_file = file.path(proj_dir, "/data/author_data/all_author_country_95CI.tsv")
ci_df = fread(ci_file)
ci_df = subset(ci_df, country != "" & !is.na(country))
ci_df = merge(un_info, ci_df)
head(ci_df)
```

    ##       country address.country_code un_region  un_subregion year bottom_CI
    ## 1 Afghanistan                   af      Asia Southern Asia 2020         0
    ## 2 Afghanistan                   af      Asia Southern Asia 2018         0
    ## 3 Afghanistan                   af      Asia Southern Asia 2006         0
    ## 4 Afghanistan                   af      Asia Southern Asia 2016         0
    ## 5 Afghanistan                   af      Asia Southern Asia 2015         0
    ## 6 Afghanistan                   af      Asia Southern Asia 2019         0
    ##   top_CI mean               corpus
    ## 1      0    0 naturenews_citations
    ## 2      0    0 naturenews_citations
    ## 3      0    0 naturenews_citations
    ## 4      0    0 naturenews_citations
    ## 5      0    0 naturenews_citations
    ## 6      0    0 naturenews_citations

``` r
#### read in the location - to - article information
all_loc_files = list.files(file.path(proj_dir, "/data/scraped_data/"), 
                            pattern="location_table_raw",
                            recursive=F,
                            full.names=T)
full_loc_df = NA
for(loc_file in all_loc_files){

    loc_df = read_corenlp_location_files(loc_file)
    loc_df$year = str_extract(loc_file, "[1-9][0-9]+") # curr_year
    loc_df$type = substring(basename(loc_file), 
                            25, nchar(basename(loc_file))-4)
    full_loc_df = rbind(full_loc_df, loc_df)
}
full_loc_df = full_loc_df[-1,]
full_loc_df = subset(full_loc_df, est_un_region != "" & 
                                        est_un_subregion != "" &
                                        est_un_region != "NO_EST" & 
                                        est_un_subregion != "NO_EST")
colnames(full_loc_df)[1] = c("address.country_code")
head(full_loc_df)
```

    ##   address.country_code
    ## 2                   ae
    ## 3                   af
    ## 4                   am
    ## 5                   am
    ## 6                   am
    ## 7                   ao
    ##                                                            file_id
    ## 2                                   science.2005.may.16.g2.science
    ## 3                          society.2005.jan.13.environment.science
    ## 4              science.2005.sep.24.drugs.thisweekssciencequestions
    ## 5                       science.2005.mar.05.spaceexploration.china
    ## 6 science.2005.sep.01.infectiousdiseases.thisweekssciencequestions
    ## 7                        science.2005.may.20.mobilephones.business
    ##                            text          ner          est_country est_un_region
    ## 2                        ramtha ORGANIZATION United Arab Emirates          Asia
    ## 3                   afghanistan      COUNTRY          Afghanistan          Asia
    ## 4                            ae         <NA>              Armenia          Asia
    ## 5 national space administration ORGANIZATION              Armenia          Asia
    ## 6                            ae         <NA>              Armenia          Asia
    ## 7                           cnn ORGANIZATION               Angola        Africa
    ##   est_un_subregion year     type
    ## 2     Western Asia 2005 guardian
    ## 3    Southern Asia 2005 guardian
    ## 4     Western Asia 2005 guardian
    ## 5     Western Asia 2005 guardian
    ## 6     Western Asia 2005 guardian
    ## 7    Middle Africa 2005 guardian

``` r
full_loc_df = subset(full_loc_df, file_id %in% file_id_keep)

#### read in all the cited articles
cited_country_file = file.path(proj_dir, 
                                "/data/author_data/cited_author_country.tsv")
cited_country_df = data.frame(fread(cited_country_file))
cited_country_df = subset(cited_country_df, country != "")
cited_country_df$country = format_country_names(cited_country_df$country)
cited_country_df = subset(cited_country_df, file_id %in% file_id_keep)

# format the countries
cited_country_df_formatted = get_author_country(cited_country_df)
cited_country_df_formatted = unique(cited_country_df_formatted)

# we only care about if a country was cited in an article, 
# not how many times it was cited
cited_country_df_formatted$num_entries = 1
```

## Process Data

### summarize the number of mentions/citations considered

``` r
# get num or articles with a country mention
mention_total = unique(subset(country_df, corpus == "naturenews_mentions", select=c(file_id, year)) )
tot_prop_mention = mention_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_mention$corpus = "naturenews_mentions"

# get num or articles with a country citation
citation_total = unique(subset(country_df, corpus == "naturenews_citations", select=c(file_id, year)) )
tot_prop_citation = citation_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_citation$corpus = "naturenews_citations"
print("num articles with citation")
```

    ## [1] "num articles with citation"

``` r
length(unique(citation_total$file_id))
```

    ## [1] 2529

``` r
length(unique(subset(country_df, corpus == "naturenews_citations")))
```

    ## [1] 8

``` r
# get num or articles with a country citation
tot_prop_df = rbind(tot_prop_mention, tot_prop_citation)
tot_prop_df = data.frame(tot_prop_df)
colnames(tot_prop_df)[2] = "tot_articles"
```

### Analysis 1: Get top bootstrap estimates for later plotting

``` r
top_countries_citation = unique(subset(ci_df, corpus == "naturenews_citations", 
                                      select=c(country, mean)) ) %>% 
                        group_by(country) %>% 
                        summarise(overall_mean=mean(mean))
top_countries_citation = top_countries_citation[
                            order(top_countries_citation$overall_mean, 
                                  decreasing=T),]
head(top_countries_citation)
```

    ## # A tibble: 6 x 2
    ##   country                    overall_mean
    ##   <chr>                             <dbl>
    ## 1 United States                     0.731
    ## 2 United Kingdom                    0.317
    ## 3 Germany                           0.197
    ## 4 France                            0.128
    ## 5 People's Republic of China        0.126
    ## 6 Australia                         0.122

``` r
# make the df into proportions for eachcountry
get_subboot <- function(country_id, curr_corpus, in_df, bootstrap_col_id="file_id"){
    bootstrap_res = compute_bootstrap_location(subset(in_df, corpus==curr_corpus), 
                                              year_col_id = "year", 
                                              article_col_id = bootstrap_col_id, 
                                              country_col_id = "country",
                                              country_agg = country_id, 
                                              conf_int = 0.95)
    bootstrap_res$country = country_id
    return(bootstrap_res)

}

# we only run the bootstraps if we want to update them
# this is an expensive process > 1hr
if(RERUN_BOOTSTRAP){
    
    BOOTSTRAP_SIZE=1000

        
    citation_country_df = NA
    for(curr_country in top_countries_citation$country[1:NUM_COUNTRIES_PLOT]){
        print(curr_country)
        res = get_subboot(curr_country, 
                          curr_corpus="naturenews_citations", 
                          country_df)
        citation_country_df = rbind(citation_country_df, res)
    }
    citation_country_df = citation_country_df[-1,]
    citation_country_df$label = ""
    citation_country_df$label[citation_country_df$year == 2020] = 
        citation_country_df$country[citation_country_df$year == 2020]
    citation_country_df$corpus = "citation"
    outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/citation_country_df.tsv")
    write.table(citation_country_df, outfile, sep="\t", quote=F, row.names=F)
    
    
    springer_country_df = NA
    for(curr_country in top_countries_citation$country[1:NUM_COUNTRIES_PLOT]){
        print(curr_country)
        res = get_subboot(curr_country, 
                          curr_corpus="springer_articles", 
                          country_df,
                          bootstrap_col_id = "doi")
        springer_country_df = rbind(springer_country_df, res)
    }
    springer_country_df = springer_country_df[-1,]
    springer_country_df$label = ""
    springer_country_df$label[springer_country_df$year == 2020] = 
        springer_country_df$country[springer_country_df$year == 2020]
    springer_country_df$corpus = "springer_last"
    outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/springer_country_df.tsv")
    write.table(springer_country_df, outfile, sep="\t", quote=F, row.names=F)
    
    
    nature_country_df = NA
    for(curr_country in top_countries_citation$country[1:NUM_COUNTRIES_PLOT]){
        print(curr_country)
        res = get_subboot(curr_country, 
                          curr_corpus="nature_articles", 
                          country_df)
        nature_country_df = rbind(nature_country_df, res)
    }
    nature_country_df = nature_country_df[-1,]
    nature_country_df$label = ""
    nature_country_df$label[nature_country_df$year == 2020] = 
        nature_country_df$country[nature_country_df$year == 2020]
    nature_country_df$corpus = "nature_last"
    outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/nature_country_df.tsv")
    write.table(nature_country_df, outfile, sep="\t", quote=F, row.names=F)
}else{
    
    citation_country_file = file.path(proj_dir,
                                      "/figure_notebooks/tmp_files/fig4_tmp/citation_country_df.tsv")
    citation_country_df = data.frame(fread(citation_country_file))

    springer_country_file = file.path(proj_dir,
                                      "/figure_notebooks/tmp_files/fig4_tmp/springer_country_df.tsv")
    springer_country_df = data.frame(fread(springer_country_file))
    
    nature_country_file = file.path(proj_dir,
                                      "/figure_notebooks/tmp_files/fig4_tmp/nature_country_df.tsv")
    nature_country_df = data.frame(fread(nature_country_file))

}
```

### Analysis 2: Identify Countries with significantly different citation and mention rates

``` r
### get the total number of mentions and citations
### for each country per year
### we will use this later as a filter, because we will only consider countries with enough observations

# for best accuracy, we only consider an article to be truely country related
# if there exists at least 2 country associated nouns in the article
loc_dups = data.frame(table(full_loc_df$file_id, full_loc_df$address.country_code))
loc_keep = subset(loc_dups, Freq > 1)
full_loc_df$freq_idx = paste(full_loc_df$file_id, full_loc_df$address.country_code, sep="_")
freq_pass = paste(loc_keep$Var1, loc_keep$Var2, sep="_")


# count the mentions by country
country_mention_total = unique(subset(country_df, 
                              corpus == "naturenews_mentions", 
                              select=c(file_id, year, address.country_code)) )
country_df_idx = paste(country_mention_total$file_id, 
                       country_mention_total$address.country_code, 
                       sep="_")
country_mention_total = country_mention_total[which(country_df_idx %in% freq_pass),]

tot_country_mention = country_mention_total %>% 
                group_by(year, address.country_code) %>% 
                summarise(n()) 
tot_country_mention$corpus = "naturenews_mentions"
colnames(tot_country_mention)[3] = "total"


# count the citations by country
country_citation_total = unique(subset(country_df, 
                               corpus == "naturenews_citations", 
                               select=c(file_id, year, address.country_code)) )
tot_country_citation = country_citation_total %>% 
                group_by(year, address.country_code) %>% 
                summarise(n()) 
tot_country_citation$corpus = "naturenews_citations"
colnames(tot_country_citation)[3] = "total"

# put them together and format
raw_sum_df = rbind(tot_country_citation, tot_country_mention)
raw_sum_df = reshape2::dcast(raw_sum_df, year+address.country_code ~ corpus, value.var="total")
raw_sum_df[is.na(raw_sum_df)] = 0
colnames(raw_sum_df)[3:4] = c("tot_citations", "tot_mentions")


###  now add in the CI estimates for each country
ci_raw_df = merge(raw_sum_df, ci_df)

# dcast the folder so we can compare mentions to citations
ci_raw_df_cast = reshape2::dcast(ci_raw_df, 
                             year+country+address.country_code+tot_citations+tot_mentions ~ corpus, 
                             value.var="mean")

###  calculate the difference between mentions + citations
ci_raw_df_cast$M_C = ci_raw_df_cast$naturenews_mentions - ci_raw_df_cast$naturenews_citations


###  now filter for the very top and bottom of citation v mention
top_diff_MC = subset(ci_raw_df_cast, tot_citations > 0 | tot_mentions > 0)
top_diff_MC_filt = NA
for(curr_year in unique(top_diff_MC$year)){
    curr_MC = subset(top_diff_MC, year == curr_year)
    top_limit = quantile(curr_MC$M_C, 0.95)
    bottom_limit = quantile(curr_MC$M_C, 0.05)
    
    print(top_limit)
    print(bottom_limit)
    
    curr_MC = subset(curr_MC, M_C > top_limit | M_C < bottom_limit)
    top_diff_MC_filt = rbind(top_diff_MC_filt, curr_MC)
}
```

    ##        95% 
    ## 0.05283693 
    ##          5% 
    ## -0.01820426 
    ##        95% 
    ## 0.04977787 
    ##          5% 
    ## -0.03974049 
    ##        95% 
    ## 0.05443141 
    ##          5% 
    ## -0.01548909 
    ##        95% 
    ## 0.04955542 
    ##          5% 
    ## -0.05322816 
    ##        95% 
    ## 0.04962592 
    ##          5% 
    ## -0.01308066 
    ##        95% 
    ## 0.06478438 
    ##          5% 
    ## -0.01481634 
    ##        95% 
    ## 0.06891625 
    ##          5% 
    ## -0.01725411 
    ##        95% 
    ## 0.05521481 
    ##          5% 
    ## -0.01155995 
    ##        95% 
    ## 0.06084481 
    ##           5% 
    ## -0.007920961 
    ##        95% 
    ## 0.07046608 
    ##           5% 
    ## -0.008982675 
    ##        95% 
    ## 0.03103717 
    ##          5% 
    ## -0.02808132 
    ##        95% 
    ## 0.03906477 
    ##          5% 
    ## -0.01732847 
    ##        95% 
    ## 0.04634396 
    ##          5% 
    ## -0.02833711 
    ##        95% 
    ## 0.03424862 
    ##          5% 
    ## -0.03013095 
    ##        95% 
    ## 0.03661231 
    ##          5% 
    ## -0.04928983 
    ##        95% 
    ## 0.07263706 
    ##           5% 
    ## -0.009492936

``` r
top_diff_MC_filt = top_diff_MC_filt[-1,]
head(top_diff_MC_filt)
```

    ##    year                    country address.country_code tot_citations
    ## 3  2005                  Australia                   au            17
    ## 7  2005                     Canada                   ca             7
    ## 11 2005 People's Republic of China                   cn             3
    ## 15 2005                    Germany                   de            29
    ## 30 2005                      India                   in             1
    ## 34 2005                      Italy                   it            12
    ##    tot_mentions naturenews_citations naturenews_mentions         M_C
    ## 3            34          0.114812162          0.09642319 -0.01838898
    ## 7            47          0.047333784          0.10108849  0.05375470
    ## 11           24          0.020500000          0.08757382  0.06707382
    ## 15           59          0.195467568          0.14756420 -0.04790337
    ## 30           20          0.006835135          0.08757019  0.08073505
    ## 34           12          0.081136486          0.05141230 -0.02972418

``` r
# make sure there are enough articles
top_diff_MC_filt = subset(top_diff_MC_filt, tot_citations > MIN_ART | tot_mentions > MIN_ART)


###  now make the 2 tables of countries that are cited more vs mentioned more
# class C vs Class M
class_c_counts = subset(top_diff_MC_filt, M_C < 0, select=c("address.country_code", "year") )
class_c_counts$class = "class_c" 
class_c_counts$idx = paste(class_c_counts$address.country_code,
                          class_c_counts$year, sep="_")
class_c_counts$idx = class_c_counts$address.country_code

class_m_counts = subset(top_diff_MC_filt, M_C > 0, select=c("address.country_code", "year") )
class_m_counts$class = "class_m" 
class_m_counts$idx = paste(class_m_counts$address.country_code,
                          class_m_counts$year, sep="_")
class_m_counts$idx = class_m_counts$address.country_code
```

### Analysis 3 part1: Identify which news articles are associated with C vs M

``` r
# for best accuracy, we only consider an article to be truely country related
# if there exists at least 2 country associated nouns in the article
loc_dups = data.frame(table(full_loc_df$file_id, full_loc_df$address.country_code))
loc_keep = subset(loc_dups, Freq > 1)
full_loc_df$freq_idx = paste(full_loc_df$file_id, full_loc_df$address.country_code, sep="_")
freq_pass = paste(loc_keep$Var1, loc_keep$Var2, sep="_")
full_mention_df = subset(full_loc_df, freq_idx %in% freq_pass)
full_mention_df$file_idx = paste(full_mention_df$address.country_code,
                      full_mention_df$year,
                      full_mention_df$file_id, sep="_")
full_mention_df$idx = paste(full_mention_df$address.country_code,
                      full_mention_df$year, sep="_")
full_mention_df$idx = full_mention_df$address.country_code


# now get the mention articles from each class
class_c_mentions = subset(full_mention_df, idx %in% 
                               class_c_counts$idx)
class_m_mentions = subset(full_mention_df, idx %in% 
                               class_m_counts$idx)

# get all the cited articles
cited_loc = merge(unique(full_mention_df[,c("file_id", "year", "type")]),
                  cited_country_df_formatted)
cited_loc$idx = paste(cited_loc$address.country_code,
                      cited_loc$year, sep="_")
cited_loc$idx = cited_loc$address.country_code
cited_loc$file_idx = paste(cited_loc$address.country_code,
                      cited_loc$year,
                      cited_loc$file_id, sep="_")

# get the cited articles from each class
class_c_citations = subset(cited_loc, idx %in% 
                               class_c_counts$idx)
class_m_citations = subset(cited_loc, idx %in% 
                               class_m_counts$idx)

# filter the mentions by the citations
class_c_mentions = subset(class_c_mentions, 
                          !file_idx %in% class_c_citations$file_idx )
class_m_mentions = subset(class_m_mentions, 
                          !file_idx %in% class_m_citations$file_idx )


# filter out 2020 for this analysis to avoid covid terms
class_c_mentions = subset(class_c_mentions, year != 2020)
class_m_mentions = subset(class_m_mentions, year != 2020)
full_mention_df = subset(full_mention_df, year != 2020)

# filter out countries that may be in both class_c and class_m
# this can be caused by mentions and citations being significantly
# different across years (sometimes M >> C, sometimes C << M)
country_overlap = intersect(class_c_mentions$address.country_code,
                            class_m_mentions$address.country_code)
class_c_mentions = subset(class_c_mentions, 
                          !address.country_code %in% country_overlap )
class_m_mentions = subset(class_m_mentions, 
                          !address.country_code %in% country_overlap )
print(country_overlap)
```

    ## [1] "ca" "ch" "cn" "de" "fr"

### Analysis 3 part2: Get the tokens associated with C vs M

``` r
#### get the word frequencies for all articles considered
class_all_word_freq = get_word_freq_per_class(full_mention_df, class_str = "class_all")


#### get the word frequencies for all articles associated with class C countries
all_country_word_freq_c = get_word_freq_per_country(class_c_mentions, 
                                                    class_str="class_c", 
                                                    class_all_word_freq,
                                                    min_freq=MIN_WORD_FREQ)
citations_freq = Reduce(function(x, y) merge(x, y, by = "word", all = T), 
                       all_country_word_freq_c)
citations_freq[is.na(citations_freq)] = 0
citations_freq$median_count = apply(citations_freq[,2:ncol(citations_freq)], 
                                   1, median)
citations_freq = citations_freq[order(citations_freq$median_count, decreasing = T),]
citations_freq = subset(citations_freq, median_count > 0)
print(knitr::kable(head(citations_freq,15), 
                       caption = "Overall Class Citation, top terms, count is per country frequency"))
```

    ## 
    ## 
    ## Table: Overall Class Citation, top terms, count is per country frequency
    ## 
    ## |word        | count_au| count_es| count_fi| count_it| count_jp| count_kr| count_nl| count_se| count_sg| count_at| count_be| count_il| count_dk| count_pt| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|------------:|
    ## |research    |     2096|      507|      190|      969|     1841|      619|     1392|      794|      323|      208|      479|      487|      215|      120|        497.0|
    ## |university  |     2234|      371|      145|      905|     1760|      538|     1422|      948|      255|      270|      330|      582|      372|       41|        455.0|
    ## |science     |     1284|      371|      117|      565|     1235|      441|      820|      321|      130|      163|      434|      329|      160|       63|        350.0|
    ## |researchers |     1572|      334|       94|      554|     1253|      286|     1168|      708|      173|      116|      270|      459|      229|       41|        310.0|
    ## |scientists  |     1128|      263|       83|      595|     1097|      228|      811|      474|      130|      127|      216|      385|      149|       44|        245.5|
    ## |data        |     1141|      242|       62|      457|      849|      192|     1168|      458|      112|      146|      141|      253|      200|       83|        221.0|
    ## |cells       |      797|       61|       25|      292|     1996|      423|      547|      697|      148|       72|       82|      569|       36|        2|        220.0|
    ## |time        |     1138|      206|       78|      480|      859|      166|      785|      581|      201|      100|      156|      387|      180|       10|        203.5|
    ## |team        |      822|      170|       59|      343|      718|      175|      570|      418|       99|       86|      131|      269|      150|       14|        172.5|
    ## |people      |      941|      138|       60|      421|      896|      148|      765|      423|      138|       93|      187|      253|      123|       10|        167.5|
    ## |human       |      521|       73|       24|      229|      664|      258|      420|      416|       52|       51|       71|      297|       76|        3|        152.5|
    ## |cell        |      411|       43|       18|      211|      975|      426|      369|      380|       67|       63|       90|      296|       21|        4|        150.5|
    ## |institute   |      504|      120|       37|      347|      765|      168|      387|      380|      123|      128|       83|      290|       82|       26|        148.0|
    ## |found       |      740|      109|       55|      262|      514|       98|      520|      401|       58|       60|      139|      252|      135|       16|        137.0|
    ## |study       |      724|       95|       82|      298|      443|       98|      563|      342|       60|       86|      130|      261|      139|       15|        134.5|

``` r
#### get the word frequencies for all articles associated with class M countries
all_country_word_freq_m = get_word_freq_per_country(class_m_mentions, 
                                                    class_str="class_m", 
                                                    class_all_word_freq,
                                                    min_freq=MIN_WORD_FREQ)
mentions_freq = Reduce(function(x, y) merge(x, y, by = "word", all = T), 
                       all_country_word_freq_m)
mentions_freq[is.na(mentions_freq)] = 0
mentions_freq$median_count = apply(mentions_freq[,2:ncol(mentions_freq)], 
                                   1, median)
mentions_freq = mentions_freq[order(mentions_freq$median_count, decreasing = T),]
mentions_freq = subset(mentions_freq, median_count > 0)
print(knitr::kable(head(mentions_freq,15), 
                       caption = "Overall Class Mention, top terms, count is per country frequency"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms, count is per country frequency
    ## 
    ## |word        | count_br| count_gb| count_in| count_us| count_mx| count_co| count_ph| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|--------:|--------:|--------:|------------:|
    ## |research    |      540|    16889|     1426|    26954|      262|      191|      227|          540|
    ## |science     |      536|    12782|     1185|    19317|      172|      115|      295|          536|
    ## |university  |      479|    13488|      844|    25425|      218|      226|      118|          479|
    ## |scientists  |      354|     8570|      718|    16115|      141|      103|      111|          354|
    ## |researchers |      344|     9625|      649|    19446|      168|      188|       81|          344|
    ## |data        |      304|     7411|      502|    15324|       36|      101|      105|          304|
    ## |time        |      214|     7993|      587|    14600|       99|      237|       84|          237|
    ## |million     |      235|     4158|      457|     9747|       54|      112|       97|          235|
    ## |government  |      231|     4067|      569|     6367|       82|       62|      111|          231|
    ## |climate     |      205|     4304|      341|     7822|      107|      229|       86|          229|
    ## |national    |      211|     3599|      453|     9110|      102|       80|       69|          211|
    ## |people      |      201|     9492|      539|    15630|      171|      151|      105|          201|
    ## |carbon      |      194|     2197|      166|     4121|       16|      337|       28|          194|
    ## |world       |      181|     5221|      443|     8377|      104|      118|      114|          181|
    ## |space       |       59|     2834|      487|     9115|        9|      175|       22|          175|

### Analysis 3 part3: Find tokens that differentiate articles related to class C / M countries

``` r
# rename the columns for merging
colnames(citations_freq)[which(colnames(citations_freq) == "median_count")] = "median_count_citations"
colnames(mentions_freq)[which(colnames(mentions_freq) == "median_count")] = "median_count_mentions"

# merge and calculate the relative counts
compare_freq = merge(subset(citations_freq, 
                            select=c("word", "median_count_citations")),
                     subset(mentions_freq, 
                            select=c("word", "median_count_mentions")))
compare_freq$compare_ratio =  compare_freq$median_count_citations /
                                compare_freq$median_count_mentions

# get the raw counts for each word, unscaled by country
class_c_word_freq = get_word_freq_per_class(
                        class_c_mentions, 
                        class_str = "class_c")
class_m_word_freq = get_word_freq_per_class(
                        class_m_mentions, 
                        class_str = "class_m")
compare_freq = merge(compare_freq, class_c_word_freq)
compare_freq = merge(compare_freq, class_m_word_freq)


# write out the tables
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=T),]
print(knitr::kable(head(compare_freq, 15), 
                       caption = "Overall Class Citation, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Citation, top terms
    ## 
    ## |word        | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:-----------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |copper      |                    8.5|                     1|      8.500000|            93|           323|
    ## |dimensional |                    8.5|                     1|      8.500000|           170|           557|
    ## |mechanics   |                    8.0|                     1|      8.000000|           120|           377|
    ## |gut         |                    7.5|                     1|      7.500000|           126|           505|
    ## |magnetic    |                   20.0|                     3|      6.666667|           398|          1485|
    ## |cancers     |                    6.5|                     1|      6.500000|           128|           717|
    ## |electrodes  |                    6.5|                     1|      6.500000|            93|           372|
    ## |therapy     |                   12.5|                     2|      6.250000|           303|          1512|
    ## |organs      |                    6.0|                     1|      6.000000|           165|           498|
    ## |austria     |                    5.5|                     1|      5.500000|           136|           151|
    ## |citation    |                   11.0|                     2|      5.500000|           186|           388|
    ## |epigenetic  |                    5.5|                     1|      5.500000|            95|           180|
    ## |parkinsons  |                    5.5|                     1|      5.500000|           169|           432|
    ## |tubes       |                    5.5|                     1|      5.500000|            63|           312|
    ## |embryo      |                   21.0|                     4|      5.250000|           288|           671|

``` r
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=F),]
print(knitr::kable(head(compare_freq, 15), 
                       caption = "Overall Class Mention, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms
    ## 
    ## |word             | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:----------------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |epa              |                    0.5|                    17|     0.0294118|            51|           802|
    ## |shuttle          |                    0.5|                    15|     0.0333333|            71|           778|
    ## |astronauts       |                    1.0|                    29|     0.0344828|            78|           838|
    ## |monsanto         |                    1.0|                    27|     0.0370370|            31|           159|
    ## |dams             |                    1.0|                    25|     0.0400000|            69|           246|
    ## |amazon           |                    3.0|                    64|     0.0468750|           157|          1172|
    ## |soils            |                    1.0|                    21|     0.0476190|            61|           300|
    ## |bush             |                    0.5|                    10|     0.0500000|            86|           787|
    ## |conservationists |                    0.5|                    10|     0.0500000|            54|           247|
    ## |hydropower       |                    0.5|                    10|     0.0500000|            17|            97|
    ## |plots            |                    0.5|                    10|     0.0500000|            59|           214|
    ## |rice             |                    3.5|                    67|     0.0522388|           159|           671|
    ## |tuition          |                    0.5|                     9|     0.0555556|            18|            98|
    ## |latin            |                    1.0|                    17|     0.0588235|            35|           224|
    ## |arsenic          |                    0.5|                     8|     0.0625000|            29|           329|

``` r
# now take the top and bottom
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=T),]
compare_freq_extreme = compare_freq
compare_freq_extreme$word_type = c(rep("Citation", sum(compare_freq$compare_ratio > 1)), 
                                   rep("Mention", sum(compare_freq$compare_ratio <= 1)))

# now lets take bootstrap estimates
word_vec = compare_freq_extreme$word

if(RERUN_BOOTSTRAP){
    BOOTSTRAP_SIZE = 1000
    start_time <- Sys.time()
    res = get_bootstrap_word_ratio(class_c_mentions, 
                                               class_m_mentions, 
                                               word_vec,
                                               conf_int = 0.95)
    bootstrap_ratio = res[[1]]
    full_boot = res[[2]]
    end_time <- Sys.time()
    elapsed_time = end_time - start_time
    print(elapsed_time)
    bootstrap_file = file.path(proj_dir,
                        "/figure_notebooks/tmp_files/fig4_tmp/fig4_bootstrap_ratio.RData")
    save(bootstrap_ratio, file = bootstrap_file)
}else{
    bootstrap_file = file.path(proj_dir,
                        "/figure_notebooks/tmp_files/fig4_tmp/fig4_bootstrap_ratio.RData")
    load(bootstrap_file)
}
# format the enrichment
bootstrap_ratio = bootstrap_ratio[order(bootstrap_ratio$mean, decreasing=T),]
compare_freq_extreme = compare_freq_extreme[order(bootstrap_ratio$mean, decreasing=T),]

bootstrap_ratio$word = factor(bootstrap_ratio$word, 
                                      levels = bootstrap_ratio$word)
compare_freq_extreme$word = factor(compare_freq_extreme$word, 
                                      levels = compare_freq_extreme$word)
bootstrap_ratio = 
    bootstrap_ratio[c(1:NUM_WORDS_REPORT,
                    (nrow(bootstrap_ratio)-NUM_WORDS_REPORT+1):nrow(bootstrap_ratio)),]
bootstrap_ratio$word_type = c(rep("Citation", NUM_WORDS_REPORT), 
                              rep("Mention", NUM_WORDS_REPORT))
compare_freq_extreme = 
    compare_freq_extreme[c(1:NUM_WORDS_REPORT,
                    (nrow(compare_freq_extreme)-NUM_WORDS_REPORT+1):nrow(compare_freq_extreme)),]
```

## Make the Figures

### make the citation plot

``` r
### full plot of citations
citation_full_gg = ggplot(citation_country_df, aes(x=as.numeric(year), y=mean,
                                                ymin=bottom_CI, ymax=top_CI,
                        fill=country, label=label)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + geom_text_repel()  + xlim(c(2005,2021)) + 
    xlab("Year of Article") + ylab("Proportion of Articles") +
    ggtitle("Proportion of Articles with at least 1 author affiliation in top 10 cited countries") + 
    scale_fill_brewer(palette="Set3") +
    theme(legend.position = "none")

ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/citation_full_gg.pdf"),
       citation_full_gg, width = 7, height = 5, units = "in", device = "pdf")

### full plot of citations with bg
cite_plot_df = Reduce(rbind, list(citation_country_df, 
                                  springer_country_df, 
                                  nature_country_df))
cite_plot_df$corpus = factor(cite_plot_df$corpus, levels = QUOTE_ANALYSIS_ORDER)
citation_indiv_10_springer_gg = ggplot(cite_plot_df, 
       aes(x=as.numeric(year), y=mean,
          ymin=bottom_CI, ymax=top_CI,
          fill=corpus, color=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw()  + xlim(c(2005,2021)) + 
    xlab("Year of Article") + ylab("Proportion of Articles") +
    ggtitle("Proportion of Articles with at least 1 Author Affiliation in Top 10 Cited Countries") + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR, labels = c("nature", "springer", "citation")) +
    scale_color_manual(values=QUOTE_ANALYSIS_COLOR, labels = c("nature", "springer", "citation")) +
    theme(legend.position = "bottom") +
    facet_wrap(~ country, scales = "free")

ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/citation_indiv_10_springer_gg.pdf"),
       citation_indiv_10_springer_gg, width = 7, height = 5, units = "in", device = "pdf")


### full plot of citations with bg top 3, no springer
cite_plot_df = Reduce(rbind, list(citation_country_df, 
                                  nature_country_df))
cite_plot_df = subset(cite_plot_df, country %in% top_countries_citation$country[1:3])
cite_plot_df$corpus = factor(cite_plot_df$corpus, levels = QUOTE_ANALYSIS_ORDER)
citation_indiv_3_gg = ggplot(cite_plot_df, 
       aes(x=as.numeric(year), y=mean,
          ymin=bottom_CI, ymax=top_CI,
          fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw()  + xlim(c(2005,2021)) + 
    xlab("Year of Article") + ylab("Proportion of Articles") +
    ggtitle("Proportion of Articles with at least 1 Author Affiliation in Top 3 Cited Countries") + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR, labels = c("nature", "citation")) +
    theme(legend.position = "bottom") +
    facet_wrap(~ country, scales = "free")

ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/citation_indiv_3_gg.pdf"),
       citation_indiv_3_gg, width = 7, height = 5, units = "in", device = "pdf")
```

### generate the mention v citation heatmap

``` r
## first make the plot with the adaptive filter results
plot_df = subset(top_diff_MC, tot_citations > MIN_ART | tot_mentions > MIN_ART)
plot_df$idx = paste(plot_df$year, plot_df$country)
plot_df$filtered = "filter"
keep_idx = paste(top_diff_MC_filt$year, top_diff_MC_filt$country)
plot_df$filtered[plot_df$idx %in% keep_idx] = "keep"

# show the spread of the difference mentions and citations
c_vs_m_filter_gg = ggplot(plot_df, aes(x=as.numeric(year), 
                        y=as.numeric(M_C),
                        color = filtered)) +
    geom_point() + theme_bw() + 
    xlab("Corpus") + ylab("Mention % - Citation % for each country+year") +
    ggtitle("Diff. btw mentions and citations for each country+year (1 point is a country)") + 
    scale_fill_brewer(palette="Set2")
ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/c_vs_m_filter_gg.pdf"),
       c_vs_m_filter_gg, width = 10, height = 5, units = "in", device = "pdf")


## make the overall heatmap
make_heatmap_res <- function(in_df, value_col){
    plot_matr_MC = reshape2::dcast(in_df, 
                             country ~ year, 
                             value.var=value_col)
    row.names(plot_matr_MC) = plot_matr_MC$country
    plot_matr_MC = plot_matr_MC[,-1]
    plot_matr_MC[is.na(plot_matr_MC)] = 0
    
    max_val = max(abs(plot_matr_MC), na.rm = T)
    breaks = c(seq(-1*max_val, max_val, by = 0.01))
    color_pmap <- colorRampPalette(c("pink", "white", "green"))(length(breaks))

    if(max_val > 1){
        breaks = c(seq(1, max_val, by = 1))
        color_pmap <- colorRampPalette(c("white", "green"))(length(breaks))

    }

    res = list(plot_matr = plot_matr_MC,
               color_pmap = color_pmap,
               breaks = breaks)
    return(res)
}

# plot the Top proportion differences
countries_considered = union(class_m_mentions$est_country,
                             class_c_mentions$est_country)
heatmap_df = subset(top_diff_MC, country %in% countries_considered)
res_MC = make_heatmap_res(heatmap_df, value_col="M_C")

# make annotation of the median difference
heatmap_median_df = heatmap_df %>% 
                group_by(country) %>% 
                summarise(median_M_C = median(M_C)) 
annot_df = data.frame(median_M_C = heatmap_median_df$median_M_C)
row.names(annot_df) = heatmap_median_df$country
annot_df$class = "Cite"
annot_df$class[annot_df$median_M_C > 0] = "Mention"
annot_df$class = factor(annot_df$class, levels = c("Mention", "Cite"))

annot_df = annot_df[order(annot_df$median_M_C),]


# now make the heatmap
res_MC$plot_matr = res_MC$plot_matr[row.names(annot_df),]
full_heatmap = pheatmap(res_MC$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation) Proportions",
         color = res_MC$color_pmap, breaks = res_MC$breaks,
         number_color = "black", annotation_row = annot_df)
```

![](figure4_files/figure-markdown_github/make_heatmap_gg-1.png)

``` r
ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/full_heatmap.pdf"),
       full_heatmap, width = 10, height = 5, units = "in", device = "pdf")
```

### generate the mention v citation word plots

``` r
word_ratio_gg = ggplot(bootstrap_ratio, aes(x=log10(bootstrap_ratio$mean), 
                                 y=as.factor(bootstrap_ratio$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + 
    geom_errorbar(aes(xmin=log10(bottom_CI), xmax=log10(top_ci)), color="black") +
    theme_bw() + 
    ylab("Words") + xlab("log10 Ratio Citation : Mention Frequencies") + 
    ggtitle("Top 15 Most Divergent Words by Class") + 
    scale_fill_brewer(palette="Set2")

ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/word_ratio_gg.pdf"),
       word_ratio_gg, width = 5, height = 5, units = "in", device = "pdf")


word_count_class_c_gg = ggplot(compare_freq_extreme, aes(x=compare_freq_extreme$class_c_count, 
                                 y=as.factor(compare_freq_extreme$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + theme_bw() + 
    ylab("Words") + xlab("Word Frequencies") + 
    ggtitle("Top 15 Frequencies for Class C") + 
    scale_fill_brewer(palette="Set2")


ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/word_count_class_c_gg.pdf"),
       word_count_class_c_gg, width = 5, height = 5, units = "in", device = "pdf")


word_count_class_m_gg = ggplot(compare_freq_extreme, aes(x=compare_freq_extreme$class_m_count, 
                                 y=as.factor(compare_freq_extreme$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + theme_bw() + 
    ylab("Words") + xlab("Word Frequencies") + 
    ggtitle("Top 15 Frequencies for Class M") + 
    scale_fill_brewer(palette="Set2")

ggsave(file.path(proj_dir, "/figure_notebooks/tmp_files/fig4_tmp/word_count_class_m_gg.pdf"),
       word_count_class_m_gg, width = 5, height = 5, units = "in", device = "pdf")
```

### format main figure

``` r
plot_overview = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/illustrator_pdfs/nature_news_mention_citation_schematic.pdf"))
plot_overview = image_annotate(plot_overview, "a", size = 20)


citation_overview_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/citation_full_gg.pdf"))
citation_overview_gg = image_annotate(citation_overview_gg, "b", size = 40)


citation_nature_indiv_sub_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/citation_indiv_3_gg.pdf"))
citation_nature_indiv_sub_gg = image_extent(citation_nature_indiv_sub_gg, '2150x1500', 
                                            color = 'white', gravity = "northeast")
citation_nature_indiv_sub_gg = image_annotate(citation_nature_indiv_sub_gg, "c", size = 40)


heatmap_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/full_heatmap.pdf"))
heatmap_gg = image_extent(heatmap_gg, '3100x1500', color = 'white', gravity = "northeast")
heatmap_gg = image_annotate(heatmap_gg, "d", size = 40)


word_ratio_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/word_ratio_gg.pdf"))
word_ratio_gg = image_annotate(word_ratio_gg, "e", size = 40)

#heatmap_gg = image_scale(heatmap_gg, 750)
#word_ratio_gg = image_scale(word_ratio_gg, 250)

middle_image <- image_append(image_scale(c(citation_overview_gg, citation_nature_indiv_sub_gg),1000), stack = FALSE)
bottom_image <- image_append(image_scale(c(heatmap_gg, word_ratio_gg), "x1000"), stack = FALSE)
full_image <- image_append(image_scale(c(plot_overview, middle_image, bottom_image), 1000), stack = TRUE)

print(full_image)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG     1000    991 sRGB       TRUE         0 300x300

<img src="figure4_files/figure-markdown_github/make_fig4-1.png" width="1000" />

``` r
outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/fig4_main.pdf")
image_write(full_image, format = "pdf", outfile)
outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/fig4_main.png")
image_write(full_image, format = "png", outfile)
```

### format supp. figure

``` r
citation_indiv_10_springer_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/citation_indiv_10_springer_gg.pdf"))
citation_indiv_10_springer_gg = image_annotate(citation_indiv_10_springer_gg, "a", size = 20)

c_vs_m_filter_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/c_vs_m_filter_gg.pdf"))
c_vs_m_filter_gg = image_annotate(c_vs_m_filter_gg, "b", size = 30)

word_count_class_c_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/word_count_class_c_gg.pdf"))
word_count_class_c_gg = image_annotate(word_count_class_c_gg, "c", size = 30)

word_count_class_m_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/tmp_files/fig4_tmp/word_count_class_m_gg.pdf"))
word_count_class_m_gg = image_annotate(word_count_class_m_gg, "d", size = 30)



bottom_image <- image_append(image_scale(c(word_count_class_c_gg, word_count_class_m_gg), "x1500"), stack = FALSE)
full_image <- image_append(image_scale(c(citation_indiv_10_springer_gg, 
                                         c_vs_m_filter_gg, 
                                         bottom_image), 3000), stack = TRUE)

print(full_image)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG     3000   5143 sRGB       TRUE         0 300x300

<img src="figure4_files/figure-markdown_github/make_supp_fig-1.png" width="3000" />

``` r
outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/fig4_supp.pdf")
image_write(full_image, format = "pdf", outfile)


outfile = file.path(proj_dir,"/figure_notebooks/tmp_files/fig4_tmp/fig4_supp.png")
image_write(full_image, format = "png", outfile)
```
