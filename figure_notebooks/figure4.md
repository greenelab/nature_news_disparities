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
#### read in the cited author data
country_file = file.path(proj_dir, "/data/author_data/all_author_country.tsv")
country_df = data.frame(fread(country_file))

# get UN info
un_info = get_country_info()
country_df = merge(country_df, un_info)
head(country_df)
```

    ##   address.country_code
    ## 1                   ad
    ## 2                   ae
    ## 3                   ae
    ## 4                   ae
    ## 5                   ae
    ## 6                   ae
    ##                                                        file_id
    ## 1                                                      446937a
    ## 2 funding-uncertainty-strands-spain-s-young-scientists-1.10177
    ## 3                                           d41586-020-03563-z
    ## 4                                                         <NA>
    ## 5                                                 468609a.html
    ## 6                                           news.2010.491.html
    ##                                                                        doi year
    ## 1                                                      doi:10.1038/446937a 2007
    ## 2 doi:10.1038/funding-uncertainty-strands-spain-s-young-scientists-1.10177 2012
    ## 3                                           doi:10.1038/d41586-020-03563-z 2020
    ## 4                                            doi:10.1007/s10967-008-1005-z 2008
    ## 5                                                 doi:10.1038/468609a.html 2010
    ## 6                                           doi:10.1038/news.2010.491.html 2010
    ##                corpus              country un_region    un_subregion
    ## 1 naturenews_mentions              Andorra    Europe Southern Europe
    ## 2 naturenews_mentions United Arab Emirates      Asia    Western Asia
    ## 3 naturenews_mentions United Arab Emirates      Asia    Western Asia
    ## 4   springer_articles United Arab Emirates      Asia    Western Asia
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
    ## 1 Afghanistan                   af      Asia Southern Asia 2012         0
    ## 2 Afghanistan                   af      Asia Southern Asia 2008         0
    ## 3 Afghanistan                   af      Asia Southern Asia 2016         0
    ## 4 Afghanistan                   af      Asia Southern Asia 2020         0
    ## 5 Afghanistan                   af      Asia Southern Asia 2015         0
    ## 6 Afghanistan                   af      Asia Southern Asia 2014         0
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

    ##   address.country_code  file_id   text          ner          est_country
    ## 2                   ae  437043a    ias ORGANIZATION United Arab Emirates
    ## 3                   ae  433471a    dgc ORGANIZATION United Arab Emirates
    ## 4                   af  433208a    pka ORGANIZATION          Afghanistan
    ## 5                   ao 4351173a    mpi ORGANIZATION               Angola
    ## 6                   ar  433114a subaru ORGANIZATION            Argentina
    ## 7                   ar  433369a müller ORGANIZATION            Argentina
    ##   est_un_region est_un_subregion year           type
    ## 2          Asia     Western Asia 2005 news-and-views
    ## 3          Asia     Western Asia 2005 news-and-views
    ## 4          Asia    Southern Asia 2005 news-and-views
    ## 5        Africa    Middle Africa 2005 news-and-views
    ## 6      Americas    South America 2005 news-and-views
    ## 7      Americas    South America 2005 news-and-views

``` r
#### read in all the cited articles
cited_country_file = file.path(proj_dir, 
                                "/data/author_data/cited_author_country.tsv")
cited_country_df = data.frame(fread(cited_country_file))
cited_country_df = subset(cited_country_df, country != "")
cited_country_df$country = format_country_names(cited_country_df$country)

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
    ##   country        overall_mean
    ##   <chr>                 <dbl>
    ## 1 United States         0.819
    ## 2 United Kingdom        0.358
    ## 3 Germany               0.260
    ## 4 France                0.167
    ## 5 Japan                 0.142
    ## 6 Canada                0.138

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
    ## 0.02259009 
    ##          5% 
    ## -0.06711471 
    ##        95% 
    ## 0.03692688 
    ##          5% 
    ## -0.02711303 
    ##        95% 
    ## 0.03964871 
    ##          5% 
    ## -0.01765817 
    ##        95% 
    ## 0.04254716 
    ##          5% 
    ## -0.03370415 
    ##        95% 
    ## 0.05228889 
    ##           5% 
    ## -0.009949715 
    ##       95% 
    ## 0.0551451 
    ##           5% 
    ## -0.009888312 
    ##        95% 
    ## 0.05571896 
    ##          5% 
    ## -0.01206763 
    ##        95% 
    ## 0.02721797 
    ##          5% 
    ## -0.02826491 
    ##        95% 
    ## 0.02761623 
    ##          5% 
    ## -0.04259051 
    ##        95% 
    ## 0.03404551 
    ##          5% 
    ## -0.04304287 
    ##        95% 
    ## 0.01790664 
    ##          5% 
    ## -0.08150624 
    ##        95% 
    ## 0.01985705 
    ##          5% 
    ## -0.07693836 
    ##        95% 
    ## 0.02462379 
    ##          5% 
    ## -0.06428023 
    ##        95% 
    ## 0.02743434 
    ##          5% 
    ## -0.06395108 
    ##        95% 
    ## 0.01901346 
    ##          5% 
    ## -0.09668138 
    ##        95% 
    ## 0.02826572 
    ##          5% 
    ## -0.07293272

``` r
top_diff_MC_filt = top_diff_MC_filt[-1,]
head(top_diff_MC_filt)
```

    ##    year   country address.country_code tot_citations tot_mentions
    ## 15 2005  Colombia                   co             1           13
    ## 19 2005   Germany                   de            79           62
    ## 27 2005    France                   fr            65           28
    ## 34 2005 Indonesia                   id             2            5
    ## 37 2005     India                   in             5           21
    ## 42 2005     Japan                   jp            65           47
    ##    naturenews_citations naturenews_mentions         M_C
    ## 15          0.003107788          0.04218688  0.03907909
    ## 19          0.246350779          0.13660416 -0.10974662
    ## 27          0.202298442          0.10505273 -0.09724572
    ## 34          0.006267290          0.04157740  0.03531011
    ## 37          0.015712773          0.08260558  0.06689281
    ## 42          0.202223676          0.09599065 -0.10623303

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

    ## [1] "fr" "gb" "us"

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
    ## |word        | count_ch| count_de| count_jp| count_nl| count_at| count_au| count_be| count_es| count_il| count_se| count_dk| count_kr| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|------------:|
    ## |university  |     1367|     3473|     1803|     1510|      308|     2280|      324|      391|      662|      954|      397|      549|        808.0|
    ## |research    |     1364|     3858|     1870|     1348|      214|     2117|      322|      481|      533|      773|      220|      620|        696.5|
    ## |cells       |      853|     1671|     2291|      613|       72|      862|       82|       53|      899|      722|       48|      409|        667.5|
    ## |researchers |     1184|     2859|     1361|     1252|      147|     1767|      275|      364|      545|      771|      245|      304|        658.0|
    ## |time        |      767|     1887|      847|      827|      125|     1125|      143|      223|      433|      569|      186|      172|        501.0|
    ## |scientists  |      787|     2276|     1037|      815|      134|     1192|      170|      262|      405|      470|      145|      227|        437.5|
    ## |cell        |      430|     1295|     1150|      423|       63|      492|       84|       35|      497|      420|       25|      432|        426.5|
    ## |science     |      643|     2045|     1194|      732|      169|     1132|      190|      319|      344|      309|      161|      456|        400.0|
    ## |human       |      444|     1456|      659|      419|       51|      645|       69|      100|      376|      407|      102|      266|        391.5|
    ## |team        |      721|     1695|      762|      650|      110|      994|      131|      215|      320|      452|      171|      194|        386.0|
    ## |data        |     1024|     2100|      857|     1239|      151|     1211|      147|      263|      271|      487|      212|      204|        379.0|
    ## |found       |      467|     1309|      517|      518|       61|      766|      139|      134|      309|      431|      138|      111|        370.0|
    ## |institute   |      576|     1745|      785|      410|      141|      551|       81|      133|      332|      370|       90|      185|        351.0|
    ## |people      |      904|     1449|      895|      717|      106|      941|      167|      141|      264|      424|      119|      149|        344.0|
    ## |study       |      557|     1165|      464|      565|      100|      767|      132|      112|      293|      391|      135|      115|        342.0|

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
    ## |word        | count_ca| count_cl| count_cn| count_co| count_in| count_br| count_mx| count_za| count_ph| count_cd| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|------------:|
    ## |research    |     2983|      139|     1819|      197|     1374|      545|      300|      754|      191|       54|        422.5|
    ## |university  |     4039|      166|     1929|      215|      805|      476|      246|      674|      113|       54|        361.0|
    ## |science     |     1760|      182|     1176|      108|     1021|      508|      188|      449|      145|       20|        318.5|
    ## |researchers |     2997|      121|     1316|      197|      631|      380|      186|      504|      107|       87|        288.5|
    ## |scientists  |     1951|       79|      969|      106|      685|      377|      182|      363|       94|       48|        272.5|
    ## |time        |     2022|      217|      826|      264|      536|      226|      111|      266|       75|       60|        245.0|
    ## |people      |     1888|       44|      818|      128|      495|      195|      182|      407|      101|      283|        239.0|
    ## |data        |     1960|       86|      726|      157|      488|      327|       38|      298|       79|       99|        227.5|
    ## |climate     |     1006|       19|      452|      322|      324|      235|      112|      166|       79|       13|        200.5|
    ## |million     |     1229|      112|      827|      153|      445|      248|       59|      250|      116|       37|        200.5|
    ## |study       |     1720|       72|      543|      140|      292|      156|       80|      214|       60|       30|        148.0|
    ## |world       |     1050|       63|      645|      127|      387|      173|      107|      164|      107|       68|        145.5|
    ## |health      |      803|       17|      545|       47|      328|       87|       53|      233|       72|      201|        144.0|
    ## |national    |      812|       86|      645|       76|      438|      211|      116|      171|       56|       37|        143.5|
    ## |government  |      788|       37|      738|       62|      548|      240|       81|      205|       59|       54|        143.0|

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
    ## |word          | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:-------------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |ips           |                   20.0|                   0.5|          40.0|           561|           117|
    ## |entanglement  |                   15.0|                   0.5|          30.0|           137|           103|
    ## |graphene      |                   14.0|                   0.5|          28.0|           226|           103|
    ## |classical     |                   25.0|                   1.0|          25.0|           228|           126|
    ## |epigenetic    |                   12.5|                   0.5|          25.0|           144|            56|
    ## |es            |                   12.0|                   0.5|          24.0|           173|            52|
    ## |mitochondrial |                   12.0|                   0.5|          24.0|           267|           113|
    ## |yeast         |                   22.5|                   1.0|          22.5|           252|           128|
    ## |cultured      |                   10.5|                   0.5|          21.0|            89|            51|
    ## |amino         |                   10.0|                   0.5|          20.0|           123|            71|
    ## |autism        |                    9.5|                   0.5|          19.0|           206|           117|
    ## |cas9          |                   19.0|                   1.0|          19.0|           228|           109|
    ## |pigs          |                   19.0|                   1.0|          19.0|           165|           130|
    ## |tubes         |                    9.0|                   0.5|          18.0|           125|            63|
    ## |higgs         |                    8.5|                   0.5|          17.0|           161|            44|

``` r
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=F),]
print(knitr::kable(head(compare_freq, 15), 
                       caption = "Overall Class Mention, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms
    ## 
    ## |word          | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:-------------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |pesticide     |                    0.5|                   5.5|     0.0909091|            20|            69|
    ## |bt            |                    0.5|                   3.0|     0.1666667|            52|           109|
    ## |spores        |                    0.5|                   3.0|     0.1666667|            19|            33|
    ## |dams          |                    1.5|                   8.5|     0.1764706|            70|           124|
    ## |indigenous    |                    2.5|                  12.5|     0.2000000|           137|           147|
    ## |rainforest    |                    0.5|                   2.5|     0.2000000|            31|           109|
    ## |tectonic      |                    0.5|                   2.5|     0.2000000|            66|            52|
    ## |plantations   |                    1.5|                   7.0|     0.2142857|            62|            51|
    ## |epa           |                    1.0|                   4.5|     0.2222222|            43|            84|
    ## |ethiopia      |                    1.0|                   4.5|     0.2222222|            37|            51|
    ## |sierra        |                    1.0|                   4.5|     0.2222222|            35|            47|
    ## |drought       |                    5.5|                  24.5|     0.2244898|           102|           210|
    ## |paulo         |                    1.5|                   6.5|     0.2307692|            27|           156|
    ## |corn          |                    2.5|                  10.5|     0.2380952|            50|            87|
    ## |deforestation |                    2.5|                  10.5|     0.2380952|           122|           300|

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
