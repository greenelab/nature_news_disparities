Mention\_v\_citation\_analysis
================
Natalie Davidson
3/31/2021

## Data Description

This analysis will compare token frequencies between two types of Nature News articles. The point of this analysis is to identify how countries are talked about differently. We seperate countries into two groups be separating them into countries that are talked about vs cited. Specifically, we first identify which countries are cited more than mentioned and which countries are mentioned more than cited. After this, we will take the most exemplary of the 2 country classes (top mentions &gt; cited: Class M & top mentions &lt; cited: Class C). We will compare the token frequencies between a mention of Class C v M.

The source data file for a bootstrap estimate of country mentions and citations: `/data/author_data/all_author_country_95CI.tsv`

The all source text is here: `/data/scraped_data/downloads/*.json`

The country mention to source articles id map here: `/data/scraped_data/location_table_raw_YEAR_ARTICLE-TYPE.tsv`

## Get Class C and M Countries

#### Read in the raw country counts.

``` r
# get the project directory, everything is set relative to this
proj_dir = here()

# read in the raw location article counts
raw_file = file.path(proj_dir, "/data/author_data/all_author_country.tsv")
raw_df = fread(raw_file)
raw_df = subset(raw_df, address.country_code != "" & !is.na(address.country_code))
raw_df = subset(raw_df, corpus %in% c("naturenews_mentions", "naturenews_citations"))

# get UN info
un_info = get_country_info()
raw_df = merge(un_info, raw_df)

head(raw_df)
```

    ##   address.country_code              country un_region    un_subregion
    ## 1                   ad              Andorra    Europe Southern Europe
    ## 2                   ae United Arab Emirates      Asia    Western Asia
    ## 3                   ae United Arab Emirates      Asia    Western Asia
    ## 4                   ae United Arab Emirates      Asia    Western Asia
    ## 5                   ae United Arab Emirates      Asia    Western Asia
    ## 6                   ae United Arab Emirates      Asia    Western Asia
    ##              file_id year               corpus
    ## 1            446937a 2007  naturenews_mentions
    ## 2 d41586-019-02846-4 2019  naturenews_mentions
    ## 3            544301a 2017 naturenews_citations
    ## 4 d41586-018-04978-5 2018 naturenews_citations
    ## 5 d41586-019-02338-5 2019  naturenews_mentions
    ## 6            480462a 2011  naturenews_mentions

``` r
# get the total number of mentions and citations
# for each country per year
# we only want to evaluate when we have > 10 in either citations or mentions
mention_total = unique(subset(raw_df, 
                              corpus == "naturenews_mentions", 
                              select=c(file_id, year, address.country_code)) )
tot_country_mention = mention_total %>% 
                group_by(year, address.country_code) %>% 
                summarise(n()) 
tot_country_mention$corpus = "naturenews_mentions"
colnames(tot_country_mention)[3] = "total"

citation_total = unique(subset(raw_df, 
                               corpus == "naturenews_citations", 
                               select=c(file_id, year, address.country_code)) )
tot_country_citation = citation_total %>% 
                group_by(year, address.country_code) %>% 
                summarise(n()) 
tot_country_citation$corpus = "naturenews_citations"
colnames(tot_country_citation)[3] = "total"

# show the spread of the mentions and citations
raw_sum_df = rbind(tot_country_citation, tot_country_mention)
ggplot(raw_sum_df, aes(x=as.factor(year), y=log10(total+1), fill=corpus)) +
    geom_boxplot() + theme_bw() + geom_hline(yintercept = log10(10), color="black") +
    xlab("Year") + ylab("log10(# Articles +1)") +
    ggtitle("log10 # of articles in each Corpus for all countries") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/read_in_raw_counts-1.png" style="display: block; margin: auto;" />

``` r
# now get the country + year pairings in a format easy to join on later
raw_sum_df = reshape2::dcast(raw_sum_df, year+address.country_code ~ corpus, value.var="total")
raw_sum_df[is.na(raw_sum_df)] = 0
colnames(raw_sum_df)[3:4] = c("tot_citations", "tot_mentions")
```

#### Read in the bootstrapped estimate of % of articles with country counts.

``` r
# read in the cited author data
ci_file = file.path(proj_dir, "/data/author_data/all_author_country_95CI.tsv")
ci_df = fread(ci_file)
ci_df = subset(ci_df, country != "" & !is.na(country))

# get UN info
un_info = get_country_info()
ci_df = merge(un_info, ci_df)

head(ci_df)
```

    ##       country address.country_code un_region  un_subregion year bottom_CI
    ## 1 Afghanistan                   af      Asia Southern Asia 2020         0
    ## 2 Afghanistan                   af      Asia Southern Asia 2016         0
    ## 3 Afghanistan                   af      Asia Southern Asia 2018         0
    ## 4 Afghanistan                   af      Asia Southern Asia 2011         0
    ## 5 Afghanistan                   af      Asia Southern Asia 2006         0
    ## 6 Afghanistan                   af      Asia Southern Asia 2014         0
    ##   top_CI mean               corpus
    ## 1      0    0 naturenews_citations
    ## 2      0    0 naturenews_citations
    ## 3      0    0 naturenews_citations
    ## 4      0    0 naturenews_citations
    ## 5      0    0 naturenews_citations
    ## 6      0    0 naturenews_citations

``` r
# merge together the article info + CI info
ci_df = merge(raw_sum_df, ci_df)

# show the spread of the mentions and citations
ggplot(ci_df, aes(x=as.factor(year), y=as.numeric(mean), fill=corpus)) +
    geom_boxplot(position="dodge") + theme_bw() + 
    xlab("Year") + ylab("Est. % of articles") +
    ggtitle("Est. % of articles in each Corpus for all countries and years") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/read_in_bootstrap-1.png" style="display: block; margin: auto;" />

``` r
# show the spread where a mentions or citations >20 
ggplot(subset(ci_df, tot_citations > MIN_ART | tot_mentions > MIN_ART), 
       aes(x=as.factor(year), y=as.numeric(mean), fill=corpus)) +
    geom_boxplot(position="dodge") + theme_bw() + 
    xlab("Year") + ylab("Est. % of articles") +
    ggtitle("Est. % of articles in each Corpus, cutoff > 20 for either mention or citation") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/read_in_bootstrap-2.png" style="display: block; margin: auto;" />

``` r
# dcast the folder so we can compare mentions to citations
ci_df_cast = reshape2::dcast(ci_df, 
                             year+country+address.country_code+tot_citations+tot_mentions ~ corpus, 
                             value.var="mean")

# calculate the difference between mentions + citations
ci_df_cast$M_C = ci_df_cast$naturenews_mentions - ci_df_cast$naturenews_citations

# show the spread of the difference mentions and citations
ggplot(subset(ci_df_cast, tot_citations > MIN_ART | tot_mentions > MIN_ART), 
       aes(x=as.numeric(year), y=as.numeric(M_C))) +
    geom_point() + theme_bw() + 
    geom_hline(yintercept = MIN_PROP, color="red") +
    geom_hline(yintercept = -1*MIN_PROP, color="red") +
    xlab("Corpus") + ylab("Mention % - Citation % for each country+year") +
    ggtitle("Diff. btw mentions and citations for each country+year (1 point is a country)") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/read_in_bootstrap-3.png" style="display: block; margin: auto;" />

``` r
# final dataframe with all filters
top_diff_MC = subset(ci_df_cast, tot_citations > MIN_ART | tot_mentions > MIN_ART)
top_diff_MC = subset(top_diff_MC, M_C > MIN_PROP | M_C < -1*MIN_PROP)
```

#### Plot the top country-year pairings have a large difference in citations vs mentions

In the previous plot, all countries that have at least one point outside of the red lines will be considered either class M (above top red line) or class C (below bottom red line). If a country is found in both groups across the years, it is removed from consideration in later downstream processing. Countries that are in both classes are shown in the heatmap as having both blue and yellow entries across the row.

``` r
make_heatmap_res <- function(in_df, value_col){
    plot_matr_MC = reshape2::dcast(in_df, 
                             country ~ year, 
                             value.var=value_col)
    row.names(plot_matr_MC) = plot_matr_MC$country
    plot_matr_MC = plot_matr_MC[,-1]
    #plot_matr_MC[is.na(plot_matr_MC)] = 0
    
    max_val = max(abs(plot_matr_MC), na.rm = T)
    breaks = c(seq(-1*max_val, max_val, by = 0.01))
    color_pmap <- colorRampPalette(c("yellow", "white", "blue"))(length(breaks))

    if(max_val > 1){
        breaks = c(seq(1, max_val, by = 1))
        color_pmap <- colorRampPalette(c("white", "blue"))(length(breaks))

    }

    res = list(plot_matr = plot_matr_MC,
               color_pmap = color_pmap,
               breaks = breaks)
    return(res)
}

# plot the Top ptoportion differences
res_MC = make_heatmap_res(top_diff_MC, value_col="M_C")
pheatmap(res_MC$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation) Proportions",
         color = res_MC$color_pmap, breaks = res_MC$breaks,
         number_color = "darkorange2")
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-1.png" style="display: block; margin: auto;" />

``` r
# OF the Top proportion differences, only plot the raw # citations
res_cite = make_heatmap_res(top_diff_MC, value_col="tot_citations")
pheatmap(res_cite$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation), total citations",
         color = res_cite$color_pmap, breaks = res_cite$breaks,
         number_color = "darkorange2")
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-2.png" style="display: block; margin: auto;" />

``` r
# OF the Top proportion differences, only plot the raw # mentions
res_mention = make_heatmap_res(top_diff_MC, value_col="tot_mentions")
pheatmap(res_mention$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation), total mentions",
         color = res_mention$color_pmap, breaks = res_mention$breaks,
         number_color = "darkorange2")
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-3.png" style="display: block; margin: auto;" />

``` r
# now make the 2 tables of countries that are cited more vs mentioned more
# class C vs Class M
class_c_counts = subset(top_diff_MC, M_C < 0, select=c("address.country_code", "year") )
class_c_counts$class = "class_c" 
class_c_counts$idx = paste(class_c_counts$address.country_code,
                          class_c_counts$year, sep="_")
class_c_counts$idx = class_c_counts$address.country_code
class_m_counts = subset(top_diff_MC, M_C > 0, select=c("address.country_code", "year") )
class_m_counts$class = "class_m" 
class_m_counts$idx = paste(class_m_counts$address.country_code,
                          class_m_counts$year, sep="_")
class_m_counts$idx = class_m_counts$address.country_code
```

## Compare tokens from articles mentioning class C and M countries

#### Get the raw text ids for each class

``` r
# for every country + year pair in our filtered table (top_diff_MC),
# get the associated raw file ids with the articles' text


# read in the location - to - article information
all_loc_files = list.files(file.path(proj_dir, "/data/scraped_data/"), 
                            pattern="location_table_raw",
                            recursive=F,
                            full.names=T)

# read in all the files
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

# for best accuracy, only include articles where a country-related noun 
# was mentinoned more than once
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
```

#### Get the cited text ids for each class

In order to only look at articles where a country is talked about and not mentioned, we need to filter the mentino articles by cited articles.

``` r
# all the cited articles
cited_country_file = file.path(proj_dir, 
                                "/data/author_data/cited_author_country.tsv")
cited_country_df = data.frame(fread(cited_country_file))
cited_country_df = subset(cited_country_df, country != "")
cited_country_df$country = format_country_names(cited_country_df$country)

# format the countries
cited_country_df_formatted = get_author_country(cited_country_df)
```

    ## Loading required package: tmaptools

``` r
cited_country_df_formatted = unique(cited_country_df_formatted)

# we only care about if a country was cited in an article, 
# not how many times it was cited
cited_country_df_formatted$num_entries = 1
```

#### Now separate the articles from class C and M

We want to separate the articles into different sections: 1) articles with class C country mentions 2) articles with class M country mentions

For both types of mention articles, we want to filter out an article from a country if it was cited in that same article.

``` r
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
    
# now get the mention articles from each class
class_c_mentions = subset(full_mention_df, idx %in% 
                               class_c_counts$idx)
class_m_mentions = subset(full_mention_df, idx %in% 
                               class_m_counts$idx)

# filter the mentions by the citations
class_c_mentions = subset(class_c_mentions, 
                          !file_idx %in% class_c_citations$file_idx )
class_m_mentions = subset(class_m_mentions, 
                          !file_idx %in% class_m_citations$file_idx )

# filter out 2020 for this analysis to avoind covid terms
class_c_mentions = subset(class_c_mentions, year != 2020)
class_m_mentions = subset(class_m_mentions, year != 2020)

# filter out countries that may be in both class_c and class_m
# this can be caused by mentions and citations being significantly
# different across years (sometimes M >> C, sometimes C << M)
country_overlap = intersect(class_c_mentions$address.country_code,
                            class_m_mentions$address.country_code)
class_c_mentions = subset(class_c_mentions, 
                          !address.country_code %in% country_overlap )
class_m_mentions = subset(class_m_mentions, 
                          !address.country_code %in% country_overlap )
```

#### Function to calculate word frequencies from raw text for each class

``` r
#' Get the word frequencies across multiple JSON files containing
#' for all years and news types
#'
#' @param class_ids This contains at a minimum a file_id - country - 
#' article_type - year mapping
#' @param class_str name of country class of interest
#' @return word_freq a dataframe of word counts across all relevant articles
get_word_freq_per_class <- function(class_ids, class_str){
        
    # get the word frequencies for class C articles
    class_word_freq = NA
    for(curr_year in unique(class_ids$year)){
        # subset for each year
        curr_year_df = subset(class_ids, year == curr_year)
        
        for(curr_type in unique(curr_year_df$type)){
            # subset for each type within a year
            curr_year_type_df = subset(curr_year_df, type == curr_type)
    
            # make the file name
            curr_file = file.path(proj_dir, "/data/scraped_data/downloads/",
                                  paste("links_crawled_", 
                                        curr_year, "_", 
                                        curr_type, ".json", sep=""))
            
            # calculate the word frequency
            curr_word_freq = calc_word_freq(curr_file, unique(curr_year_type_df$file_id))
            curr_word_freq = as.data.frame(curr_word_freq)
            curr_word_freq$year = curr_year
            curr_word_freq$type = curr_type
            class_word_freq = rbind(class_word_freq, curr_word_freq)
        }
    }
    class_word_freq_total = class_word_freq[-1,]
    class_word_freq = class_word_freq[-1,]
    
    # sum word frequencies across the different JSON files
    class_word_freq = class_word_freq %>%
                    select("word", "n") %>%
                    group_by(word) %>% 
                    summarise(sum(n)) 
    col_id = paste(class_str, "_count", sep="")
    colnames(class_word_freq)[2] = col_id
    
    # sort
    class_word_freq = class_word_freq[
                            order(class_word_freq[,col_id], 
                                  decreasing=T),]
     
    return(class_word_freq)
}


# get the word frequencies for each class of country
class_all_word_freq = get_word_freq_per_class(full_mention_df, class_str = "class_all")

print(head(class_all_word_freq))
```

    ## # A tibble: 6 x 2
    ##   word        class_all_count
    ##   <chr>                 <int>
    ## 1 research              38300
    ## 2 university            36852
    ## 3 researchers           31780
    ## 4 science               23086
    ## 5 scientists            22078
    ## 6 data                  21857

## Calculate word frequencies for each class

Here, we will go through each country, finding the words most specific to articles mentioning this country, but not citing it. This is calculated by finding terms that have the highest the larget ratio: term in country specific articles / term over all articles. Then, we will take the top 100 terms per country, and see which terms show up the most across all countries. This will give us an indication of how class C countries are talked about vs how class M countries are talked about.

These are calculated on a per country basis, because the number of articles per country are very different.

#### Class C

``` r
all_country_word_freq_c = list()
# write out top words for each country
for(curr_country in unique(class_c_mentions$address.country_code)){
    
    # get the word freq for class C mentions
    class_c_word_freq = get_word_freq_per_class(
                            subset(class_c_mentions, address.country_code == curr_country), 
                            class_str = "class_c")
     
    # merge with the word freq for entire corpus
     per_class_word_freq = merge(data.table(class_c_word_freq), 
                                data.table(class_all_word_freq), by="word")
     
        
    # word should be used at least 100 time in the full corpus
    per_class_word_freq = subset(per_class_word_freq, class_all_count > 100)
        
    # get the word frequency scaled by corpus frequency
    per_class_word_freq$ratio = per_class_word_freq$class_c_count / 
                                per_class_word_freq$class_all_count
      
    # write out top words per country
    per_class_word_freq = per_class_word_freq[order(per_class_word_freq$ratio, decreasing=T),]
    print(knitr::kable(head(per_class_word_freq,15), 
                 caption = paste(curr_country, "Class Citation, top terms")))
    
    # save top words
    per_class_word_freq_df = per_class_word_freq[,c("word", "class_c_count")]
    colnames(per_class_word_freq_df)[2] = paste("count", curr_country, sep="_")
    all_country_word_freq_c[[curr_country]] = per_class_word_freq_df
    

}
```

    ## 
    ## 
    ## Table: ca Class Citation, top terms
    ## 
    ## |word      | class_c_count| class_all_count|     ratio|
    ## |:---------|-------------:|---------------:|---------:|
    ## |blogentry |           118|             124| 0.9516129|
    ## |alberta   |           185|             218| 0.8486239|
    ## |ontario   |           246|             308| 0.7987013|
    ## |sands     |            90|             115| 0.7826087|
    ## |edmonton  |            86|             111| 0.7747748|
    ## |canadas   |           154|             207| 0.7439614|
    ## |ottawa    |           109|             150| 0.7266667|
    ## |ut        |           141|             203| 0.6945813|
    ## |mcgill    |           117|             176| 0.6647727|
    ## |canadian  |           392|             627| 0.6251994|
    ## |tar       |            75|             122| 0.6147541|
    ## |canada    |          1466|            2433| 0.6025483|
    ## |ddt       |            62|             105| 0.5904762|
    ## |fraser    |            68|             120| 0.5666667|
    ## |newline   |           123|             221| 0.5565611|
    ## 
    ## 
    ## Table: ch Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |msf           |            71|             132| 0.5378788|
    ## |eth           |            72|             137| 0.5255474|
    ## |basel         |            88|             185| 0.4756757|
    ## |lausanne      |            69|             155| 0.4451613|
    ## |novartis      |            92|             217| 0.4239631|
    ## |zurich        |           161|             405| 0.3975309|
    ## |swiss         |           219|             623| 0.3515249|
    ## |perovskite    |            41|             117| 0.3504274|
    ## |roche         |            74|             212| 0.3490566|
    ## |switzerland   |           435|            1266| 0.3436019|
    ## |athletes      |            44|             155| 0.2838710|
    ## |astrocytes    |            32|             116| 0.2758621|
    ## |rnai          |            59|             239| 0.2468619|
    ## |olympic       |            27|             110| 0.2454545|
    ## |immunotherapy |            33|             137| 0.2408759|
    ## 
    ## 
    ## Table: de Class Citation, top terms
    ## 
    ## |word       | class_c_count| class_all_count|     ratio|
    ## |:----------|-------------:|---------------:|---------:|
    ## |dfg        |           112|             125| 0.8960000|
    ## |germanys   |           231|             322| 0.7173913|
    ## |leipzig    |            99|             139| 0.7122302|
    ## |munich     |           194|             315| 0.6158730|
    ## |pbo        |            68|             119| 0.5714286|
    ## |heidelberg |           118|             208| 0.5673077|
    ## |potsdam    |            73|             130| 0.5615385|
    ## |bonn       |            85|             159| 0.5345912|
    ## |germany    |          1474|            3062| 0.4813847|
    ## |max        |           445|             926| 0.4805616|
    ## |planck     |           453|            1003| 0.4516451|
    ## |bullying   |            79|             175| 0.4514286|
    ## |philae     |            55|             122| 0.4508197|
    ## |ludwig     |            51|             114| 0.4473684|
    ## |german     |           476|            1078| 0.4415584|
    ## 
    ## 
    ## Table: fr Class Citation, top terms
    ## 
    ## |word         | class_c_count| class_all_count|     ratio|
    ## |:------------|-------------:|---------------:|---------:|
    ## |blogentry    |           118|             124| 0.9516129|
    ## |iter         |           364|             419| 0.8687351|
    ## |cnrs         |           161|             201| 0.8009950|
    ## |frances      |           153|             241| 0.6348548|
    ## |newline      |           132|             221| 0.5972851|
    ## |philae       |            63|             122| 0.5163934|
    ## |le           |           105|             204| 0.5147059|
    ## |thymus       |            54|             121| 0.4462810|
    ## |fusion       |           480|            1084| 0.4428044|
    ## |ut           |            86|             203| 0.4236453|
    ## |purification |            63|             150| 0.4200000|
    ## |priming      |            42|             102| 0.4117647|
    ## |pasteur      |            61|             150| 0.4066667|
    ## |france       |           617|            1521| 0.4056542|
    ## |cirm         |           100|             256| 0.3906250|
    ## 
    ## 
    ## Table: it Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |laquila     |           101|             136| 0.7426471|
    ## |italys      |           110|             175| 0.6285714|
    ## |milan       |            65|             126| 0.5158730|
    ## |italian     |           267|             569| 0.4692443|
    ## |rome        |           123|             311| 0.3954984|
    ## |italy       |           380|            1083| 0.3508772|
    ## |lisa        |            75|             264| 0.2840909|
    ## |virgo       |            47|             179| 0.2625698|
    ## |shale       |            56|             244| 0.2295082|
    ## |prosecutors |            28|             131| 0.2137405|
    ## |sasso       |            25|             126| 0.1984127|
    ## |fermi       |            43|             219| 0.1963470|
    ## |cultivation |            27|             144| 0.1875000|
    ## |gran        |            25|             146| 0.1712329|
    ## |microglia   |            25|             150| 0.1666667|
    ## 
    ## 
    ## Table: jp Class Citation, top terms
    ## 
    ## |word      | class_c_count| class_all_count|     ratio|
    ## |:---------|-------------:|---------------:|---------:|
    ## |obokata   |           107|             112| 0.9553571|
    ## |blogentry |           118|             124| 0.9516129|
    ## |jaxa      |           137|             149| 0.9194631|
    ## |hayabusa  |           106|             116| 0.9137931|
    ## |cdb       |            92|             110| 0.8363636|
    ## |stap      |           106|             132| 0.8030303|
    ## |riken     |           222|             298| 0.7449664|
    ## |tohoku    |            95|             128| 0.7421875|
    ## |fukushima |           265|             369| 0.7181572|
    ## |yamanaka  |           126|             177| 0.7118644|
    ## |japans    |           395|             568| 0.6954225|
    ## |ut        |           130|             203| 0.6403941|
    ## |tokyo     |           375|             622| 0.6028939|
    ## |newline   |           123|             221| 0.5565611|
    ## |takahashi |            57|             103| 0.5533981|
    ## 
    ## 
    ## Table: at Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |austria       |            95|             214| 0.4439252|
    ## |austrian      |            46|             105| 0.4380952|
    ## |afm           |            28|             102| 0.2745098|
    ## |vienna        |            69|             293| 0.2354949|
    ## |wooden        |            24|             126| 0.1904762|
    ## |glacier       |            42|             295| 0.1423729|
    ## |citizen       |            32|             239| 0.1338912|
    ## |iaea          |            18|             138| 0.1304348|
    ## |confocal      |            13|             106| 0.1226415|
    ## |census        |            23|             197| 0.1167513|
    ## |radioactivity |            16|             142| 0.1126761|
    ## |twisted       |            12|             115| 0.1043478|
    ## |caesium       |            17|             183| 0.0928962|
    ## |glaciers      |            60|             661| 0.0907716|
    ## |nitrate       |             9|             107| 0.0841121|
    ## 
    ## 
    ## Table: au Class Citation, top terms
    ## 
    ## |word         | class_c_count| class_all_count|     ratio|
    ## |:------------|-------------:|---------------:|---------:|
    ## |csiro        |           172|             180| 0.9555556|
    ## |aus          |           160|             197| 0.8121827|
    ## |australias   |           207|             301| 0.6877076|
    ## |oxytocin     |            76|             126| 0.6031746|
    ## |melbourne    |           209|             362| 0.5773481|
    ## |commonwealth |            65|             114| 0.5701754|
    ## |queensland   |           111|             199| 0.5577889|
    ## |brisbane     |            63|             119| 0.5294118|
    ## |sydney       |           194|             367| 0.5286104|
    ## |canberra     |           116|             230| 0.5043478|
    ## |australian   |           464|             921| 0.5038002|
    ## |australia    |           961|            2129| 0.4513856|
    ## |cdb          |            47|             110| 0.4272727|
    ## |tectonics    |            52|             134| 0.3880597|
    ## |uc           |            49|             134| 0.3656716|
    ## 
    ## 
    ## Table: be Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |owen          |            48|             125| 0.3840000|
    ## |transgender   |            40|             116| 0.3448276|
    ## |belgium       |           101|             311| 0.3247588|
    ## |shale         |            75|             244| 0.3073770|
    ## |archaeopteryx |            31|             108| 0.2870370|
    ## |beer          |            54|             221| 0.2443439|
    ## |consciousness |            36|             177| 0.2033898|
    ## |fracking      |            16|             102| 0.1568627|
    ## |conscious     |            34|             229| 0.1484716|
    ## |ali           |            20|             141| 0.1418440|
    ## |citizen       |            31|             239| 0.1297071|
    ## |quasars       |            15|             117| 0.1282051|
    ## |phytoplankton |            18|             145| 0.1241379|
    ## |tipping       |            16|             137| 0.1167883|
    ## |testosterone  |            19|             163| 0.1165644|
    ## 
    ## 
    ## Table: es Class Citation, top terms
    ## 
    ## |word      | class_c_count| class_all_count|     ratio|
    ## |:---------|-------------:|---------------:|---------:|
    ## |spanish   |           140|             327| 0.4281346|
    ## |cuba      |            45|             133| 0.3383459|
    ## |barcelona |            56|             167| 0.3353293|
    ## |spain     |           177|             661| 0.2677761|
    ## |jellyfish |            45|             222| 0.2027027|
    ## |grb       |            23|             114| 0.2017544|
    ## |tehran    |            20|             102| 0.1960784|
    ## |iodide    |            19|             102| 0.1862745|
    ## |paintings |            26|             145| 0.1793103|
    ## |cas       |            35|             215| 0.1627907|
    ## |madrid    |            35|             223| 0.1569507|
    ## |nio       |            45|             293| 0.1535836|
    ## |redshift  |            25|             182| 0.1373626|
    ## |lin       |            18|             135| 0.1333333|
    ## |english   |            72|             564| 0.1276596|
    ## 
    ## 
    ## Table: nl Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |leiden      |           117|             152| 0.7697368|
    ## |priming     |            58|             102| 0.5686275|
    ## |ams         |            58|             130| 0.4461538|
    ## |netherlands |           492|            1112| 0.4424460|
    ## |ut          |            88|             203| 0.4334975|
    ## |sofia       |            49|             137| 0.3576642|
    ## |bullying    |            52|             175| 0.2971429|
    ## |dutch       |           103|             370| 0.2783784|
    ## |rankings    |            36|             149| 0.2416107|
    ## |unconscious |            27|             125| 0.2160000|
    ## |topological |            41|             191| 0.2146597|
    ## |hansen      |            31|             152| 0.2039474|
    ## |amsterdam   |            50|             249| 0.2008032|
    ## |citation    |            85|             425| 0.2000000|
    ## |odour       |            25|             126| 0.1984127|
    ## 
    ## 
    ## Table: se Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |macchiarini   |           120|             121| 0.9917355|
    ## |uppsala       |            89|             119| 0.7478992|
    ## |karolinska    |           126|             185| 0.6810811|
    ## |ki            |            71|             110| 0.6454545|
    ## |ut            |           116|             203| 0.5714286|
    ## |lund          |            73|             133| 0.5488722|
    ## |sweden        |           295|             600| 0.4916667|
    ## |swedish       |           107|             292| 0.3664384|
    ## |astrocytes    |            40|             116| 0.3448276|
    ## |stockholm     |            91|             313| 0.2907348|
    ## |archaeopteryx |            31|             108| 0.2870370|
    ## |chickens      |            51|             237| 0.2151899|
    ## |proteomics    |            26|             126| 0.2063492|
    ## |phosphate     |            43|             213| 0.2018779|
    ## |gatherers     |            19|             101| 0.1881188|
    ## 
    ## 
    ## Table: dk Class Citation, top terms
    ## 
    ## |word       | class_c_count| class_all_count|     ratio|
    ## |:----------|-------------:|---------------:|---------:|
    ## |denmark    |           119|             363| 0.3278237|
    ## |danish     |            40|             141| 0.2836879|
    ## |greenland  |            96|             527| 0.1821632|
    ## |willerslev |            20|             132| 0.1515152|
    ## |citizen    |            33|             239| 0.1380753|
    ## |copenhagen |            72|             548| 0.1313869|
    ## |offshore   |            40|             311| 0.1286174|
    ## |biosphere  |            14|             110| 0.1272727|
    ## |grids      |            16|             128| 0.1250000|
    ## |bedrock    |            15|             121| 0.1239669|
    ## |seabed     |            16|             132| 0.1212121|
    ## |plague     |            22|             190| 0.1157895|
    ## |cambrian   |            15|             131| 0.1145038|
    ## |turbines   |            24|             211| 0.1137441|
    ## |retirement |            26|             238| 0.1092437|

``` r
citations_freq = Reduce(merge, all_country_word_freq_c)
citations_freq$median_count = apply(citations_freq[,2:ncol(citations_freq)], 
                                   1, median)
citations_freq = citations_freq[order(citations_freq$median_count, decreasing = T),]
print(knitr::kable(head(citations_freq,15), 
                       caption = "Overall Class Citation, top terms, count is per country frequency"))
```

    ## 
    ## 
    ## Table: Overall Class Citation, top terms, count is per country frequency
    ## 
    ## |word        | count_ca| count_ch| count_de| count_fr| count_it| count_jp| count_at| count_au| count_be| count_es| count_nl| count_se| count_dk| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|------------:|
    ## |research    |     2948|     1283|     3682|     2283|      921|     1769|      204|     2003|      319|      466|     1303|      718|      205|         1283|
    ## |university  |     3882|     1218|     3260|     1308|      876|     1650|      270|     2051|      300|      354|     1376|      857|      360|         1218|
    ## |researchers |     2824|     1041|     2637|     1172|      551|     1215|      115|     1498|      251|      323|     1145|      678|      223|         1041|
    ## |scientists  |     1898|      702|     2114|     1040|      574|      990|      125|     1045|      162|      246|      766|      421|      148|          702|
    ## |data        |     1922|      925|     1951|      693|      452|      835|      145|     1119|      136|      240|     1150|      448|      198|          693|
    ## |people      |     1830|      806|     1333|      723|      377|      831|       91|      800|      158|      131|      690|      368|      113|          690|
    ## |time        |     2000|      684|     1744|     1013|      448|      862|       99|     1002|      138|      187|      747|      530|      174|          684|
    ## |science     |     1773|      603|     2016|     1089|      452|     1152|      158|     1080|      184|      311|      714|      282|      157|          603|
    ## |cells       |     1240|      693|     1565|      706|      288|     1762|       72|      725|       78|       49|      530|      573|       35|          573|
    ## |team        |     1445|      582|     1481|      535|      325|      677|       82|      765|      117|      160|      542|      382|      149|          535|
    ## |study       |     1597|      486|     1040|      416|      294|      417|       88|      674|      125|       95|      522|      312|      126|          416|
    ## |found       |     1417|      396|     1161|      457|      257|      473|       57|      645|      125|      110|      476|      343|      127|          396|
    ## |human       |     1027|      413|     1258|      475|      224|      590|       51|      475|       67|       65|      380|      379|       74|          380|
    ## |institute   |      991|      518|     1570|      605|      341|      735|      126|      485|       77|      120|      379|      335|       81|          379|
    ## |technology  |      730|      412|      873|      406|      125|      602|       76|      427|       58|       96|      368|      195|       49|          368|

#### Class M

``` r
# first remove all cited articles from the articles with a mention

all_country_word_freq_m = list()
# write out top words for each country
for(curr_country in unique(class_m_mentions$address.country_code)){
    
    # get the word freq for class C mentions
    class_m_word_freq = get_word_freq_per_class(
                            subset(class_m_mentions, address.country_code == curr_country), 
                            class_str = "class_m")
     
    # merge with the word freq for entire corpus
     per_class_word_freq = merge(data.table(class_m_word_freq), 
                                data.table(class_all_word_freq), by="word")
     
        
    # word should be used at least 100 time in the full corpus
    per_class_word_freq = subset(per_class_word_freq, class_all_count > 100)
        
    # get the word frequency scaled by corpus frequency
    per_class_word_freq$ratio = per_class_word_freq$class_m_count / 
                                per_class_word_freq$class_all_count
      
    # write out top words per country
    per_class_word_freq = per_class_word_freq[order(per_class_word_freq$ratio, decreasing=T),]
    print(knitr::kable(head(per_class_word_freq,15), 
                 caption = paste(curr_country, "Class Mention, top terms")))
    
    # save top words
    per_class_word_freq_df = per_class_word_freq[,c("word", "class_m_count")]
    colnames(per_class_word_freq_df)[2] = paste("count", curr_country, sep="_")
    all_country_word_freq_m[[curr_country]] = per_class_word_freq_df

    

}
```

    ## 
    ## 
    ## Table: co Class Mention, top terms
    ## 
    ## |word           | class_m_count| class_all_count|     ratio|
    ## |:--------------|-------------:|---------------:|---------:|
    ## |ut             |            94|             203| 0.4630542|
    ## |ppm            |            48|             134| 0.3582090|
    ## |shuttles       |            29|             104| 0.2788462|
    ## |carbonate      |            64|             239| 0.2677824|
    ## |foam           |            42|             159| 0.2641509|
    ## |sinks          |            33|             137| 0.2408759|
    ## |colombia       |            27|             119| 0.2268908|
    ## |sequestration  |            33|             167| 0.1976048|
    ## |grasses        |            24|             131| 0.1832061|
    ## |shuttle        |           111|             610| 0.1819672|
    ## |soot           |            19|             122| 0.1557377|
    ## |anthropogenic  |            24|             157| 0.1528662|
    ## |boron          |            22|             148| 0.1486486|
    ## |hansen         |            22|             152| 0.1447368|
    ## |photosynthetic |            18|             126| 0.1428571|
    ## 
    ## 
    ## Table: in Class Mention, top terms
    ## 
    ## |word      | class_m_count| class_all_count|     ratio|
    ## |:---------|-------------:|---------------:|---------:|
    ## |blogentry |           118|             124| 0.9516129|
    ## |bangalore |            99|             117| 0.8461538|
    ## |indias    |           435|             570| 0.7631579|
    ## |delhi     |           215|             287| 0.7491289|
    ## |rupees    |            69|             109| 0.6330275|
    ## |pachauri  |            78|             126| 0.6190476|
    ## |newline   |           129|             221| 0.5837104|
    ## |rao       |            83|             144| 0.5763889|
    ## |indian    |           681|            1324| 0.5143505|
    ## |india     |           912|            1950| 0.4676923|
    ## |tiger     |           101|             239| 0.4225941|
    ## |singh     |            65|             166| 0.3915663|
    ## |tigers    |            62|             165| 0.3757576|
    ## |himalayas |            39|             124| 0.3145161|
    ## |shale     |            76|             244| 0.3114754|
    ## 
    ## 
    ## Table: mx Class Mention, top terms
    ## 
    ## |word           | class_m_count| class_all_count|     ratio|
    ## |:--------------|-------------:|---------------:|---------:|
    ## |bison          |            63|             132| 0.4772727|
    ## |mexicos        |            55|             127| 0.4330709|
    ## |mexican        |            96|             232| 0.4137931|
    ## |rust           |            51|             181| 0.2817680|
    ## |wheat          |           110|             476| 0.2310924|
    ## |maize          |           119|             531| 0.2241055|
    ## |maya           |            29|             133| 0.2180451|
    ## |varieties      |            89|             485| 0.1835052|
    ## |mexico         |           279|            1556| 0.1793059|
    ## |monsanto       |            30|             169| 0.1775148|
    ## |tolerant       |            23|             131| 0.1755725|
    ## |nanotechnology |            52|             333| 0.1561562|
    ## |autonomous     |            39|             259| 0.1505792|
    ## |herbicide      |            19|             133| 0.1428571|
    ## |transgenic     |            55|             505| 0.1089109|
    ## 
    ## 
    ## Table: ph Class Mention, top terms
    ## 
    ## |word           | class_m_count| class_all_count|     ratio|
    ## |:--------------|-------------:|---------------:|---------:|
    ## |philippines    |            46|             175| 0.2628571|
    ## |rice           |           161|             757| 0.2126816|
    ## |geoengineering |            28|             210| 0.1333333|
    ## |irrigation     |            18|             147| 0.1224490|
    ## |rust           |            22|             181| 0.1215470|
    ## |bt             |            27|             259| 0.1042471|
    ## |cotton         |            28|             288| 0.0972222|
    ## |taylor         |            34|             368| 0.0923913|
    ## |monsanto       |            14|             169| 0.0828402|
    ## |wheat          |            38|             476| 0.0798319|
    ## |sheep          |            17|             226| 0.0752212|
    ## |opera          |             8|             115| 0.0695652|
    ## |tolerant       |             9|             131| 0.0687023|
    ## |humanitarian   |             8|             119| 0.0672269|
    ## |varieties      |            32|             485| 0.0659794|

``` r
mentions_freq = Reduce(merge, all_country_word_freq_m)
mentions_freq$median_count = apply(mentions_freq[,2:ncol(mentions_freq)], 
                                   1, median)
mentions_freq = mentions_freq[order(mentions_freq$median_count, decreasing = T),]

print(knitr::kable(head(mentions_freq,15), 
                       caption = "Overall Class Mention, top terms, count is per country frequency"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms, count is per country frequency
    ## 
    ## |word        | count_co| count_in| count_mx| count_ph| median_count|
    ## |:-----------|--------:|--------:|--------:|--------:|------------:|
    ## |research    |      197|     1368|      261|      183|        229.0|
    ## |university  |      215|      800|      217|      102|        216.0|
    ## |climate     |      329|      322|      108|       78|        215.0|
    ## |time        |      267|      594|       98|       66|        182.5|
    ## |researchers |      197|      628|      166|       79|        181.5|
    ## |science     |      108|     1020|      173|      142|        157.5|
    ## |people      |      128|      490|      163|       97|        145.5|
    ## |million     |      154|      444|       58|      103|        128.5|
    ## |water       |      189|      229|       46|       68|        128.5|
    ## |scientists  |      107|      680|      138|       83|        122.5|
    ## |global      |      196|      264|       38|       44|        120.0|
    ## |data        |      159|      483|       35|       77|        118.0|
    ## |change      |      155|      285|       76|       54|        115.5|
    ## |world       |      127|      386|      102|      104|        115.5|
    ## |plants      |      162|      134|       77|       55|        105.5|

## Calculate the difference in word frequencies between Class C and M using the top words for each class

Here we will calculate the most descriminative words between the two classes, using the country balanced `median_count`.

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
print(knitr::kable(head(compare_freq,15), 
                       caption = "Overall Class Citation, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Citation, top terms
    ## 
    ## |word          | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:-------------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |quantum       |                    147|                   4.5|      32.66667|          2318|            47|
    ## |physicists    |                     95|                   3.5|      27.14286|          1027|            43|
    ## |virus         |                    113|                   5.0|      22.60000|          1028|            89|
    ## |machines      |                     33|                   1.5|      22.00000|           390|            28|
    ## |theoretical   |                     54|                   2.5|      21.60000|           563|            36|
    ## |journals      |                     62|                   3.0|      20.66667|           907|            81|
    ## |bird          |                     56|                   3.0|      18.66667|           374|            20|
    ## |software      |                     55|                   3.0|      18.33333|           762|            42|
    ## |clinical      |                    174|                   9.5|      18.31579|          1461|           174|
    ## |articles      |                     36|                   2.0|      18.00000|           463|            35|
    ## |sophisticated |                     27|                   1.5|      18.00000|           266|            15|
    ## |pigs          |                     17|                   1.0|      17.00000|           172|             8|
    ## |reading       |                     17|                   1.0|      17.00000|           206|            18|
    ## |vaccines      |                     51|                   3.0|      17.00000|           446|           127|
    ## |tissues       |                     33|                   2.0|      16.50000|           333|            12|

``` r
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=F),]
print(knitr::kable(head(compare_freq,15), 
                       caption = "Overall Class Mention, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms
    ## 
    ## |word              | median_count_citations| median_count_mentions| compare_ratio| class_c_count| class_m_count|
    ## |:-----------------|----------------------:|---------------------:|-------------:|-------------:|-------------:|
    ## |varieties         |                      8|                  32.0|     0.2500000|           162|           134|
    ## |drought           |                      6|                  19.5|     0.3076923|           122|            79|
    ## |soils             |                      6|                  19.5|     0.3076923|           125|            72|
    ## |rice              |                     14|                  45.0|     0.3111111|           209|           215|
    ## |farmers           |                     31|                  82.0|     0.3780488|           378|           237|
    ## |crops             |                     32|                  77.0|     0.4155844|           529|           224|
    ## |grain             |                      3|                   7.0|     0.4285714|            45|            23|
    ## |reductions        |                      4|                   8.5|     0.4705882|           114|            37|
    ## |yields            |                     10|                  20.5|     0.4878049|           171|            72|
    ## |environmentalists |                      3|                   5.5|     0.5454545|            91|            19|
    ## |crop              |                     21|                  36.0|     0.5833333|           298|           147|
    ## |villages          |                      7|                  12.0|     0.5833333|            99|            59|
    ## |tonnes            |                     17|                  28.5|     0.5964912|           374|           114|
    ## |coal              |                     12|                  20.0|     0.6000000|           294|           131|
    ## |agricultural      |                     31|                  50.0|     0.6200000|           384|           181|

``` r
# now take the top and bottom
compare_freq = compare_freq[order(compare_freq$compare_ratio, decreasing=T),]
compare_freq_extreme = compare_freq[c(1:15,(nrow(compare_freq)-14):nrow(compare_freq)),]
compare_freq_extreme$word_type = c(rep("Citation", 15), rep("Mention", 15))

# show the enrichment
compare_freq_extreme$word = factor(compare_freq_extreme$word, 
                                      levels = compare_freq_extreme$word)
ggplot(compare_freq_extreme, aes(x=log10(compare_freq_extreme$compare_ratio), 
                                 y=as.factor(compare_freq_extreme$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + theme_bw() + 
    ylab("Words") + xlab("log10 Ratio Citation : Mention Frequencies") + 
    ggtitle("log10 Ratio Citation : Mention Frequencies for most extreme words, normalized by country") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/calc_word_freq_diff-1.png" style="display: block; margin: auto;" />

``` r
ggplot(compare_freq_extreme, aes(x=compare_freq_extreme$class_c_count, 
                                 y=as.factor(compare_freq_extreme$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + theme_bw() + 
    ylab("Words") + xlab("Word Frequencies") + 
    ggtitle("Frequencies for Class C countries for most extreme words") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/calc_word_freq_diff-2.png" style="display: block; margin: auto;" />

``` r
ggplot(compare_freq_extreme, aes(x=compare_freq_extreme$class_m_count, 
                                 y=as.factor(compare_freq_extreme$word),
                                 fill=word_type)) +
    geom_bar(stat="identity") + theme_bw() + 
    ylab("Words") + xlab("Word Frequencies") + 
    ggtitle("Frequencies for Class M countries for most extreme words") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/calc_word_freq_diff-3.png" style="display: block; margin: auto;" />
