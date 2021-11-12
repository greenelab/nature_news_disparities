Fig3\_name\_origin
================
Natalie Davidson
5/3/2021

## Overview

This notebook generates figure 3 and additional supplemental figures.

The **data** it uses to build the plots are here:

This document compares two "foreground" datasets (estimated name origin of authors quoted + cited in nature news articles) and compares it to two possible "background" datasets (random sampling of 2.4K Springer articles, and all nature articles)

The quote data file is: `./data/author_data/all_speaker_fullname_pred.tsv` The names mentioned data file is: `./data/author_data/all_mentioned_fullname_pred.tsv` The bg data file is: `./data/author_data/all_author_fullname_pred.tsv`

The three corpi are indexed by the `corpus` column:

1.  `news_quotes`: **foreground** est. name origin of Nature News quoted speaker

2.  `nature_last`: **background** est. name origin of last author of Nature articles.

3.  `springer_last`: **background** est. name origin of last author of a random subset of Springer articles.

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

# second read in the names mentioned data
name_pred_file = file.path(proj_dir, 
                             "/data/author_data/all_mentioned_fullname_pred.tsv")
name_info_file = file.path(proj_dir, 
                             "/data/author_data/all_mentioned_fullname.tsv")

mentioned_name_df = read_name_origin(name_pred_file, name_info_file)
mentioned_name_df$name_origin[mentioned_name_df$name_origin == "Jewish"] = "Hebrew"
mentioned_name_df = subset(mentioned_name_df, !file_id %in% too_many_quotes_file_id)


# now read in the BG data
name_pred_file = file.path(proj_dir, 
                         "/data/author_data/all_author_fullname_pred.tsv")
name_info_file = file.path(proj_dir, 
                         "/data/author_data/all_author_fullname.tsv")
cite_name_df = read_name_origin(name_pred_file, name_info_file)
cite_name_df$name_origin[cite_name_df$name_origin == "Jewish"] = "Hebrew"



# we will only use last authors in the citations
cite_name_df = subset(cite_name_df, author_pos == "last")

# format the corpus for consistent naming across figures
cite_name_df$corpus[cite_name_df$corpus == "springer_articles"] = "springer_last"
cite_name_df$corpus[cite_name_df$corpus == "nature_articles"] = "nature_last"

# seperate out citations from columns by journalists vs scientists
journalist_idx = which(cite_name_df$corpus == "naturenews_citations" &
                         cite_name_df$file_id %in% news_df$file_id)
scientist_idx = which(cite_name_df$corpus == "naturenews_citations" &
                         !cite_name_df$file_id %in% news_df$file_id)
cite_name_df$corpus[journalist_idx] = "citation_journalist"
cite_name_df$corpus[scientist_idx] = "citation_scientist"

# now we want to join these two datasets together
# we assume a quote is comparable to a publication
# so we will have a quote set as a doi
quote_name_df$doi = quote_name_df$quote
quote_name_df$corpus = "quote"
quote_name_df$corpus[which(quote_name_df$type == "guardian")] = "guardian_quote"

# we assume a name mentioned is comparable to a publication
# so we will have a name + file_id as a doi
mentioned_name_df$doi = paste(mentioned_name_df$author, 
                              mentioned_name_df$file_id, 
                              sep="_")
mentioned_name_df$corpus = "mention"
mentioned_name_df$corpus[which(mentioned_name_df$type == "guardian")] = "guardian_mention"

# filter the article types we don't want to use
quote_name_df = subset(quote_name_df, !type %in% c("career-column", "news-and-views"))
mentioned_name_df = subset(mentioned_name_df, !type %in% c("career-column", "news-and-views"))


col_ids = c("author", "year", "name_origin", "corpus", "doi")
name_df = rbind(cite_name_df[,col_ids], 
                quote_name_df[,col_ids], 
                mentioned_name_df[,col_ids])
head(name_df)
```

    ##               author year   name_origin              corpus
    ## 7      Aaltje Jansen 2012      European       springer_last
    ## 16   Aarne Oikarinen 2010        Nordic       springer_last
    ## 18       Aaron Bauer 2014      European       springer_last
    ## 23     Aaron Clauset 2020      European citation_journalist
    ## 25 Aaron Cohen-Gadol 2011 CelticEnglish       springer_last
    ## 30       Aaron Evans 2006 CelticEnglish  citation_scientist
    ##                               doi
    ## 7     doi:10.1186/1471-2318-12-19
    ## 16             doi:10.1186/cc8938
    ## 18    doi:10.1186/1471-213X-14-29
    ## 23 doi:10.1038/s41467-019-08746-5
    ## 25  doi:10.1007/s00381-011-1423-z
    ## 30           doi:10.1038/35078008

``` r
name_df = unique(name_df)
```

## Process Data

### summarize the number of articles/quotes/citations considered in each corpus

``` r
citation_j_total = unique(subset(name_df, corpus == "citation_journalist", select=c(doi, year)) )
tot_prop_citation_j = citation_j_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_citation_j$corpus = "citation_journalist"

citation_s_total = unique(subset(name_df, corpus == "citation_scientist", select=c(doi, year)) )
tot_prop_citation_s = citation_s_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_citation_s$corpus = "citation_scientist"

quote_total = unique(subset(name_df, corpus == "quote", select=c(doi, year)) )
tot_prop_quote = quote_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_quote$corpus = "quote"

quote_total = unique(subset(name_df, corpus == "guardian_quote", select=c(doi, year)) )
tot_prop_g_quote = quote_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_g_quote$corpus = "guardian_quote"

springer_total = unique(subset(name_df, corpus == "springer_last", select=c(doi, year)) )
tot_prop_springer = springer_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_springer$corpus = "springer_last"

nature_total = unique(subset(name_df, corpus == "nature_last", select=c(doi, year)) )
tot_prop_nature = nature_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_nature$corpus = "nature_last"


mention_total = unique(subset(name_df, corpus == "mention", select=c(doi, year)) )
tot_prop_mention = mention_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_mention$corpus = "mention"

mention_total = unique(subset(name_df, corpus == "guardian_mention", select=c(doi, year)) )
tot_prop_g_mention = mention_total %>% 
                group_by(year) %>% 
                summarise(n()) 
tot_prop_g_mention$corpus = "guardian_mention"

num_art_tot = Reduce(rbind, list(tot_prop_citation_j, 
                                 tot_prop_citation_s,
                                 tot_prop_quote,
                                 tot_prop_g_quote,
                                 tot_prop_springer, 
                                 tot_prop_nature,
                                 tot_prop_mention,
                                 tot_prop_g_mention))
num_art_tot = data.frame(num_art_tot)
colnames(num_art_tot)[2] = "tot_articles"

print("total of observations")
```

    ## [1] "total of observations"

``` r
num_art_tot %>% 
    group_by(corpus) %>% 
    summarise(n()) 
```

    ## # A tibble: 8 x 2
    ##   corpus              `n()`
    ##   <chr>               <int>
    ## 1 citation_journalist    16
    ## 2 citation_scientist     16
    ## 3 guardian_mention       16
    ## 4 guardian_quote         16
    ## 5 mention                16
    ## 6 nature_last            16
    ## 7 quote                  16
    ## 8 springer_last          16

``` r
print("median of observations")
```

    ## [1] "median of observations"

``` r
num_art_tot %>% 
    group_by(corpus) %>% 
    summarise(median(tot_articles)) 
```

    ## # A tibble: 8 x 2
    ##   corpus              `median(tot_articles)`
    ##   <chr>                                <dbl>
    ## 1 citation_journalist                   267 
    ## 2 citation_scientist                    660.
    ## 3 guardian_mention                     3271 
    ## 4 guardian_quote                       2898 
    ## 5 mention                              4726.
    ## 6 nature_last                           679 
    ## 7 quote                                5662 
    ## 8 springer_last                        1684.

``` r
print("min of observations")
```

    ## [1] "min of observations"

``` r
num_art_tot %>% 
    group_by(corpus) %>% 
    summarise(min(tot_articles)) 
```

    ## # A tibble: 8 x 2
    ##   corpus              `min(tot_articles)`
    ##   <chr>                             <int>
    ## 1 citation_journalist                 139
    ## 2 citation_scientist                  503
    ## 3 guardian_mention                   2192
    ## 4 guardian_quote                     2240
    ## 5 mention                            3177
    ## 6 nature_last                         565
    ## 7 quote                              3751
    ## 8 springer_last                      1298

### Get bootstrap estimates

``` r
# helper method for calling the bootstrap
get_subboot <- function(origin_id, curr_corpus, in_df, bootstrap_col_id="doi"){
    bootstrap_res = compute_bootstrap_location(subset(in_df, 
                                                      corpus==curr_corpus), 
                                              year_col_id = "year", 
                                              article_col_id = bootstrap_col_id, 
                                              country_col_id = "name_origin",
                                              country_agg = origin_id, 
                                              conf_int = 0.95)
    bootstrap_res$name_origin = origin_id
    
    # add a label for plotting later
    bootstrap_res$label[bootstrap_res$year == 2020] = 
        bootstrap_res$name_origin[bootstrap_res$year == 2020]
        

    return(bootstrap_res)

}

if(RERUN_BOOTSTRAP){
    
    # get the bootstrapped CI for each source data type
    citation_j_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="citation_journalist",
                          name_df)
        citation_j_origin_df = rbind(citation_j_origin_df, res)
    }
    citation_j_origin_df = citation_j_origin_df[-1,]
    
    citation_s_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="citation_scientist",
                          name_df)
        citation_s_origin_df = rbind(citation_s_origin_df, res)
    }
    citation_s_origin_df = citation_s_origin_df[-1,]
    

    
    
    quote_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="quote",
                          name_df)
        quote_origin_df = rbind(quote_origin_df, res)
    }
    quote_origin_df = quote_origin_df[-1,]
    
    g_quote_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="guardian_quote",
                          name_df)
        g_quote_origin_df = rbind(g_quote_origin_df, res)
    }
    g_quote_origin_df = g_quote_origin_df[-1,]

    springer_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="springer_last",
                          name_df)
        springer_origin_df = rbind(springer_origin_df, res)
    }
    springer_origin_df = springer_origin_df[-1,]
    
    nature_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="nature_last",
                          name_df)
        nature_origin_df = rbind(nature_origin_df, res)
    }
    nature_origin_df = nature_origin_df[-1,]
    
    mention_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="mention",
                          name_df)
        mention_origin_df = rbind(mention_origin_df, res)
    }
    mention_origin_df = mention_origin_df[-1,]
    
    g_mention_origin_df = NA
    for(curr_origin in unique(name_df$name_origin)){
        print(curr_origin)
        res = get_subboot(curr_origin, 
                          curr_corpus="guardian_mention",
                          name_df)
        g_mention_origin_df = rbind(g_mention_origin_df, res)
    }
    g_mention_origin_df = g_mention_origin_df[-1,]

    # re-add corpus column for easy reference later
    citation_j_origin_df$corpus = "citation_journalist"
    citation_s_origin_df$corpus = "citation_scientist"
    quote_origin_df$corpus = "quote"
    g_quote_origin_df$corpus = "guardian_quote"
    springer_origin_df$corpus = "springer_last"
    nature_origin_df$corpus = "nature_last"
    mention_origin_df$corpus = "mention"
    g_mention_origin_df$corpus = "guardian_mention"

    all_bootstrap_df = Reduce(rbind, list(quote_origin_df,
                                          g_quote_origin_df,
                                       citation_j_origin_df,
                                       citation_s_origin_df,
                                       nature_origin_df,
                                       springer_origin_df,
                                       mention_origin_df,
                                       g_mention_origin_df))
    all_bootstrap_df$corpus = factor(all_bootstrap_df$corpus, levels = QUOTE_ANALYSIS_ORDER)
    
    outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/all_bootstrap_df.tsv")
    write.table(all_bootstrap_df, outfile, sep="\t", quote=F, row.names=F)
}else{
    
    all_bootstrap_file = file.path(proj_dir,
                                      "/figure_notebooks/manuscript_figs/fig3_tmp/all_bootstrap_df.tsv")
    all_bootstrap_df = data.frame(fread(all_bootstrap_file))
    
    citation_j_origin_df = subset(all_bootstrap_df, corpus == "citation_journalist")
    citation_s_origin_df = subset(all_bootstrap_df, corpus == "citation_scientist")
    quote_origin_df = subset(all_bootstrap_df, corpus == "quote")
    g_quote_origin_df = subset(all_bootstrap_df, corpus == "guardian_quote")
    springer_origin_df = subset(all_bootstrap_df, corpus == "springer_last")
    nature_origin_df = subset(all_bootstrap_df, corpus == "nature_last")
    mention_origin_df = subset(all_bootstrap_df, corpus == "mention")
    g_mention_origin_df = subset(all_bootstrap_df, corpus == "guardian_mention")
    
}

print("range of European and CelticEnglish names")
```

    ## [1] "range of European and CelticEnglish names"

``` r
summary(subset(citation_j_origin_df, 
               name_origin %in% c("European", "CelticEnglish"))$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2480  0.3185  0.3353  0.3382  0.3617  0.4134

``` r
summary(subset(citation_s_origin_df, 
               name_origin %in% c("European", "CelticEnglish"))$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2654  0.3148  0.3676  0.3550  0.3993  0.4301

``` r
print("range of East names")
```

    ## [1] "range of East names"

``` r
summary(subset(citation_j_origin_df, 
               name_origin == "EastAsian")$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.05732 0.11168 0.14414 0.14659 0.17382 0.24845

``` r
summary(subset(citation_s_origin_df, 
               name_origin == "EastAsian")$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.09758 0.11725 0.13309 0.13492 0.15222 0.17043

``` r
summary(subset(quote_origin_df, 
               name_origin == "EastAsian")$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04928 0.05464 0.06448 0.06268 0.06819 0.07446

``` r
summary(subset(g_quote_origin_df, 
               name_origin == "EastAsian")$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.01562 0.02505 0.03150 0.03134 0.03586 0.04867

``` r
print("range of non European or non CelticEnglish or non EastAsian names")
```

    ## [1] "range of non European or non CelticEnglish or non EastAsian names"

``` r
summary(subset(citation_j_origin_df, 
               !name_origin %in% c("European", "CelticEnglish", "EastAsian"))$mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00000 0.01118 0.02108 0.02530 0.03722 0.08139

``` r
summary(subset(citation_s_origin_df, 
               !name_origin %in% c("European", "CelticEnglish", "EastAsian"))$mean)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.003277 0.011912 0.021403 0.022150 0.031179 0.050843

## Make the Figures

### generate the overview plot

``` r
#### Overview plot of the number of considered "articles" by type
num_art_tot$corpus = factor(num_art_tot$corpus, levels = QUOTE_ANALYSIS_ORDER)
tot_art_gg = ggplot(num_art_tot, aes(x=as.numeric(year), y=tot_articles,
                              fill=corpus, color=corpus)) +
    geom_point() + geom_line() + theme_bw() + 
    xlab("Year of Article") + ylab("Number of Total Articles/Quotes/Citations/Mentions") +
    ggtitle("Total number of Articles/Quotes/Citations/Mentions per Corpus") + 
    scale_color_manual(values=QUOTE_ANALYSIS_COLOR) +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/tot_art_gg.pdf"),
       tot_art_gg, width = 5, height = 5, units = "in", device = "pdf")
```

### generate the citation plots

``` r
#### plot the overview of name origin by citation
citation_j_overview_gg = ggplot(citation_j_origin_df, aes(x=as.numeric(year), y=mean,
                          ymin=bottom_CI, ymax=top_CI,
                          fill=name_origin, label=label)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + geom_text_repel() + xlim(c(2005,2021))  +
    xlab("Year of Article") + ylab("Proportion of Journalist Citations") +
    ggtitle("Est. Proportion of the Cited Last Author Name Origin (journalist written)") + 
    scale_fill_brewer(palette="Set2") +
    theme(legend.position = "none")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_j_overview_gg.pdf"),
       citation_j_overview_gg, width = 7, height = 5, units = "in", device = "pdf")

citation_s_overview_gg = ggplot(citation_s_origin_df, aes(x=as.numeric(year), y=mean,
                          ymin=bottom_CI, ymax=top_CI,
                          fill=name_origin, label=label)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + geom_text_repel() + xlim(c(2005,2021))  +
    xlab("Year of Article") + ylab("Proportion of Scientist Citations") +
    ggtitle("Est. Proportion of the Cited Last Author Name Origin (scientist written)") + 
    scale_fill_brewer(palette="Set2") +
    theme(legend.position = "none")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_s_overview_gg.pdf"),
       citation_s_overview_gg, width = 7, height = 5, units = "in", device = "pdf")

# plot by each name origin individually
citation_nature_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% c("nature_last", 
                                                       "citation_journalist",
                                                       "citation_scientist")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Citations or Articles") +
    ggtitle(paste("Percentage Citations vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_nature_indiv_full_gg.pdf"),
       citation_nature_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")


# plot by each name origin individually
citation_springer_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% c("springer_last", 
                                                       "citation_journalist",
                                                       "citation_scientist")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Citations or Articles") +
    ggtitle(paste("Percentage Citations vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_springer_indiv_full_gg.pdf"),
       citation_springer_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")



citation_j_nature_indiv_sub_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% c("nature_last", 
                                                       "citation_journalist") &
                                         name_origin %in% c("CelticEnglish", "EastAsian", "European")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Citations or Articles") +
    ggtitle(paste("Prop. Citations vs Last Authorship by Name Origin in Journalist Written Articles")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin, dir="h", scales="free") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_j_nature_indiv_sub_gg.pdf"),
       citation_j_nature_indiv_sub_gg, width = 7, height = 5, units = "in", device = "pdf")

citation_s_nature_indiv_sub_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% c("nature_last", 
                                                       "citation_scientist") &
                                         name_origin %in% c("CelticEnglish", "EastAsian", "European")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Citations or Articles") +
    ggtitle(paste("Prop. Citations vs Last Authorship by Name Origin in Scientist Written Articles")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin, dir="h", scales="free") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/citation_s_nature_indiv_sub_gg.pdf"),
       citation_s_nature_indiv_sub_gg, width = 7, height = 5, units = "in", device = "pdf")
```

### generate the quote plots

``` r
#### plot the overview of name origin by quoted speaker
quote_overview_gg = ggplot(quote_origin_df, aes(x=as.numeric(year), y=mean,
                          ymin=bottom_CI, ymax=top_CI,
                          fill=name_origin, label=label)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + geom_text_repel() + xlim(c(2005,2021))  +
    xlab("Year of Article") + ylab("Proportion of Quotes") +
    ggtitle("Est. Proportion of Quoted Speakers' Name Origin") + 
    scale_fill_brewer(palette="Set2") +
    theme(legend.position = "none")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/quote_overview_gg.pdf"),
       quote_overview_gg, width = 7, height = 5, units = "in", device = "pdf")


# plot by each name origin individually
quote_nature_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% 
                                             c("nature_last", "quote", "guardian_quote")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Quotes or Articles") +
    ggtitle(paste("Percentage Quotes vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap( ~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/quote_nature_indiv_full_gg.pdf"),
       quote_nature_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")

quote_springer_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% 
                                             c("springer_last", "quote", "guardian_quote")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Quotes or Articles") +
    ggtitle(paste("Percentage Quotes vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap( ~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/quote_springer_indiv_full_gg.pdf"),
       quote_springer_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")


quote_nature_indiv_sub_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% 
                                             c("nature_last", "quote", "guardian_quote") &
                                         name_origin %in% c("CelticEnglish", "EastAsian", "European")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Quotes or Articles") +
    ggtitle(paste("Percentage Quotes vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin) +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/quote_nature_indiv_sub_gg.pdf"),
       quote_nature_indiv_sub_gg, width = 7, height = 5, units = "in", device = "pdf")
```

### generate the mention plots

``` r
#### plot the overview of name origin by mentioned person
mention_overview_gg = ggplot(mention_origin_df, aes(x=as.numeric(year), y=mean,
                          ymin=bottom_CI, ymax=top_CI,
                          fill=name_origin, label=label)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + geom_text_repel() + xlim(c(2005,2021))  +
    xlab("Year of Article") + ylab("Proportion of Mentioned Names") +
    ggtitle("Est. Proportion of Mentioned Speakers' Name Origin") + 
    scale_fill_brewer(palette="Set2") +
    theme(legend.position = "none")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/mention_overview_gg.pdf"),
       mention_overview_gg, width = 7, height = 5, units = "in", device = "pdf")


# plot by each name origin individually
mention_nature_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% 
                                             c("nature_last", "mention", "guardian_mention")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Mentions or Articles") +
    ggtitle(paste("Percentage Mentions vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap( ~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/mention_nature_indiv_full_gg.pdf"),
       mention_nature_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")

mention_springer_indiv_full_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in% 
                                             c("springer_last", "mention", "guardian_mention")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Mentions or Articles") +
    ggtitle(paste("Percentage Mentions vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap( ~ name_origin, scales = "free_y") +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/mention_springer_indiv_full_gg.pdf"),
       mention_springer_indiv_full_gg, width = 7, height = 5, units = "in", device = "pdf")


mention_nature_indiv_sub_gg = ggplot(subset(all_bootstrap_df, 
                                         corpus %in%
                                             c("nature_last", "mention", "guardian_mention") &
                                         name_origin %in% c("CelticEnglish", "EastAsian", "European")), 
                                aes(x=as.numeric(year), y=mean,
                                      ymin=bottom_CI, ymax=top_CI,
                                      fill=corpus)) +
    geom_point() + geom_ribbon(alpha=0.5) + geom_line(alpha=0.5) +
    theme_bw() + 
    xlab("Year of Article") + ylab("Percentage Mentions or Articles") +
    ggtitle(paste("Percentage Mentions vs Last Authorship by Name Origin")) + 
    scale_fill_manual(values=QUOTE_ANALYSIS_COLOR) +
    facet_wrap(~ name_origin) +
    theme(legend.position="bottom")

ggsave(file.path(proj_dir, "/figure_notebooks/manuscript_figs/fig3_tmp/mention_nature_indiv_sub_gg.pdf"),
       mention_nature_indiv_sub_gg, width = 7, height = 5, units = "in", device = "pdf")
```

### format main figure

``` r
plot_overview = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/illustrator_pdfs/nature_news_name_origin_schematic.pdf"))
plot_overview = image_annotate(plot_overview, "a", size = 20)


citation_j_nature_indiv_sub_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_j_nature_indiv_sub_gg.pdf"))
citation_j_nature_indiv_sub_gg = image_annotate(citation_j_nature_indiv_sub_gg, "b", size = 30)


citation_s_nature_indiv_sub_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_s_nature_indiv_sub_gg.pdf"))
citation_s_nature_indiv_sub_gg = image_annotate(citation_s_nature_indiv_sub_gg, "c", size = 30)



quote_nature_indiv_sub_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/quote_nature_indiv_sub_gg.pdf"))
quote_nature_indiv_sub_gg = image_annotate(quote_nature_indiv_sub_gg, "d", size = 30)


mention_nature_indiv_sub_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/mention_nature_indiv_sub_gg.pdf"))
mention_nature_indiv_sub_gg = image_annotate(mention_nature_indiv_sub_gg, "e", size = 30)


bottom_image <- image_append(image_scale(c(quote_nature_indiv_sub_gg, 
                                           mention_nature_indiv_sub_gg),3000), stack = FALSE)
middle_image <- image_append(image_scale(c(citation_j_nature_indiv_sub_gg,
                                           citation_s_nature_indiv_sub_gg),3000), stack = FALSE)
full_image <- image_append(image_scale(c(plot_overview, middle_image, bottom_image), 3000), stack = TRUE)


print(full_image)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG     3000   3140 sRGB       TRUE         0 300x300

<img src="figure3_files/figure-markdown_github/make_fig1-1.png" width="3000" />

``` r
outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_main.pdf")
image_write(full_image, format = "pdf", outfile)

outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_main.png")
image_write(full_image, format = "png", outfile)
```

### format supp. figure 3

``` r
tot_art_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/tot_art_gg.pdf"))
tot_art_gg = image_annotate(tot_art_gg, "a", size = 20)

citation_j_overview_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_j_overview_gg.pdf"))
citation_j_overview_gg = image_annotate(citation_j_overview_gg, "b", size = 30)


citation_s_overview_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_s_overview_gg.pdf"))
citation_s_overview_gg = image_annotate(citation_s_overview_gg, "c", size = 30)


quote_overview_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/quote_overview_gg.pdf"))
quote_overview_gg = image_annotate(quote_overview_gg, "d", size = 30)


mention_overview_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/mention_overview_gg.pdf"))
mention_overview_gg = image_annotate(mention_overview_gg, "e", size = 30)


bottom_image <- image_append(image_scale(c(quote_overview_gg, 
                                           mention_overview_gg),3000), stack = FALSE)
middle_image <- image_append(image_scale(c(citation_j_overview_gg,
                                           citation_s_overview_gg),3000), stack = FALSE)
full_image <- image_append(c(image_scale(tot_art_gg, 1500), 
                             image_scale(c(middle_image, bottom_image), 3000)), stack = TRUE)
                           

print(full_image)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG     3000   3644 sRGB       TRUE         0 300x300

<img src="figure3_files/figure-markdown_github/make_supp_fig-1.png" width="3000" />

``` r
outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_supp.pdf")
image_write(full_image, format = "pdf", outfile)
outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_supp.png")
image_write(full_image, format = "png", outfile)
```

### format supp. figure 4

``` r
citation_nature_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_nature_indiv_full_gg.pdf"))
citation_nature_indiv_full_gg = image_annotate(citation_nature_indiv_full_gg, "a", size = 30)


citation_springer_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/citation_springer_indiv_full_gg.pdf"))
citation_springer_indiv_full_gg = image_annotate(citation_springer_indiv_full_gg, "b", size = 30)


quote_nature_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/quote_nature_indiv_full_gg.pdf"))
quote_nature_indiv_full_gg = image_annotate(quote_nature_indiv_full_gg, "c", size = 30)


quote_springer_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/quote_springer_indiv_full_gg.pdf"))
quote_springer_indiv_full_gg = image_annotate(quote_springer_indiv_full_gg, "d", size = 30)


mention_nature_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/mention_nature_indiv_full_gg.pdf"))
mention_nature_indiv_full_gg = image_annotate(mention_nature_indiv_full_gg, "e", size = 30)


mention_springer_indiv_full_gg = image_read_pdf(file.path(proj_dir,
                                  "/figure_notebooks/manuscript_figs/fig3_tmp/mention_springer_indiv_full_gg.pdf"))
mention_springer_indiv_full_gg = image_annotate(mention_springer_indiv_full_gg, "f", size = 30)



top_image <- image_append(image_scale(c(citation_nature_indiv_full_gg,
                                           citation_springer_indiv_full_gg),3000), stack = FALSE)
middle_image <- image_append(image_scale(c(quote_nature_indiv_full_gg,
                                           quote_springer_indiv_full_gg),3000), stack = FALSE)
bottom_image <- image_append(image_scale(c(mention_nature_indiv_full_gg, 
                                           mention_springer_indiv_full_gg),3000), stack = FALSE)
full_image <- image_append(image_scale(c(top_image, middle_image, bottom_image), 3000), stack = TRUE)

print(full_image)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG     3000   3216 sRGB       TRUE         0 300x300

<img src="figure3_files/figure-markdown_github/make_supp_fig_mentions-1.png" width="3000" />

``` r
outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_supp2.pdf")
image_write(full_image, format = "pdf", outfile)
outfile = file.path(proj_dir,"/figure_notebooks/manuscript_figs/fig3_tmp/fig3_supp2.png")
image_write(full_image, format = "png", outfile)
```
