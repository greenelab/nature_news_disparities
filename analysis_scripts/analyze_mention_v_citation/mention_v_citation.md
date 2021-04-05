Mention\_v\_citation\_analysis
================
Natalie Davidson
3/31/2021

## Data Description

This analysis will compare token frequencies between two types of Nature News articles. To identify the types of articles, we first identify which countries are cited more than mentioned and which countries are mentioned more than cited. This comparison is done on a per year basis. After this, we will take the most exemplary of the 2 country classes (top mentions &gt; cited: Class M & top mentions &lt; cited: Class C). We will compare the token frequencies between a mention of Class C v M.

The source data file for a bootstrap estimate of country mentions and citations: `/data/author_data/all_author_country_95CI.tsv`

The all source text is here: `/data/scraped_data/downloads/*.json`

The country mention to source articles id map here: `/data/scraped_data/location_table_raw_YEAR_ARTICLE-TYPE.tsv`

## Get Top Class C and M Countries

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
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
tot_country_mention$corpus = "naturenews_mentions"
colnames(tot_country_mention)[3] = "total"

citation_total = unique(subset(raw_df, 
                               corpus == "naturenews_citations", 
                               select=c(file_id, year, address.country_code)) )
tot_country_citation = citation_total %>% 
                group_by(year, address.country_code) %>% 
                summarise(n()) 
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
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
# now filter for only the country-year pairings that have enough counts
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
    ggtitle("Diff. between mentions and citations for each country and year") + 
    scale_fill_brewer(palette="Set2")
```

<img src="mention_v_citation_files/figure-markdown_github/read_in_bootstrap-3.png" style="display: block; margin: auto;" />

``` r
# final dataframe with all filters
top_diff_MC = subset(ci_df_cast, tot_citations > MIN_ART | tot_mentions > MIN_ART)
top_diff_MC = subset(top_diff_MC, M_C > MIN_PROP | M_C < -1*MIN_PROP)
```

#### Plot the top country-year pairings have a large difference in citations vs mentions

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
         color = res_MC$color_pmap, breaks = res_MC$breaks)
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-1.png" style="display: block; margin: auto;" />

``` r
# OF the Top proportion differences, only plot the raw # citations
res_cite = make_heatmap_res(top_diff_MC, value_col="tot_citations")
pheatmap(res_cite$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation), total citations",
         color = res_cite$color_pmap, breaks = res_cite$breaks)
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-2.png" style="display: block; margin: auto;" />

``` r
# OF the Top proportion differences, only plot the raw # mentions
res_mention = make_heatmap_res(top_diff_MC, value_col="tot_mentions")
pheatmap(res_mention$plot_matr, cluster_rows = F, 
         cluster_cols = F, display_numbers = T, 
         main = "Top (Mention - Citation), total mentions",
         color = res_mention$color_pmap, breaks = res_mention$breaks)
```

<img src="mention_v_citation_files/figure-markdown_github/plot_filtered_countries-3.png" style="display: block; margin: auto;" />

## Compare tokens from Class C and M Country-Year pairs

#### Get the raw text ids for each class

``` r
# for every country + year pair in our filtered table (top_diff_MC), get the associated raw file ids
class_c_counts = subset(top_diff_MC, M_C < 0 )
class_c_counts$idx = paste(class_c_counts$address.country_code, 
                             class_c_counts$year, sep="_")
class_m_counts = subset(top_diff_MC, M_C > 0 )
class_m_counts$idx = paste(class_m_counts$address.country_code, 
                             class_m_counts$year, sep="_")

# find all location estimates
all_loc_files = list.files(file.path(proj_dir, "/data/scraped_data/"), 
                            pattern="location_table_raw",
                            recursive=F,
                            full.names=T)

# read in all the file id's
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

# now get the file ids per class
country_year_pass = unique(top_diff_MC[,c("year", "address.country_code")])
colnames(full_loc_df)[1] = c("address.country_code")
full_loc_df_pass = merge(country_year_pass, full_loc_df) 
full_loc_df_pass$idx = paste(full_loc_df_pass$address.country_code, 
                             full_loc_df_pass$year, sep="_")

# this means that its not a year-country pair, but only a country over all time
class_c_ids = subset(full_loc_df_pass, address.country_code %in% unique(class_c_counts$address.country_code))
class_m_ids = subset(full_loc_df_pass, address.country_code %in% unique(class_m_counts$address.country_code))

head(class_c_ids)
```

    ##   year address.country_code        file_id
    ## 1 2005                   ch        437468a
    ## 2 2005                   ch 051010-13.html
    ## 3 2005                   ch        438151a
    ## 4 2005                   ch        436456a
    ## 5 2005                   ch 050221-17.html
    ## 6 2005                   ch 050124-12.html
    ##                                    text          ner est_country est_un_region
    ## 1 swiss federal institute of technology ORGANIZATION Switzerland        Europe
    ## 2                           switzerland      COUNTRY Switzerland        Europe
    ## 3                  university of zürich ORGANIZATION Switzerland        Europe
    ## 4   university of geneva medical centre ORGANIZATION Switzerland        Europe
    ## 5                           switzerland      COUNTRY Switzerland        Europe
    ## 6                  world economic forum ORGANIZATION Switzerland        Europe
    ##   est_un_subregion         type     idx
    ## 1   Western Europe news-feature ch_2005
    ## 2   Western Europe         news ch_2005
    ## 3   Western Europe news-feature ch_2005
    ## 4   Western Europe news-feature ch_2005
    ## 5   Western Europe         news ch_2005
    ## 6   Western Europe         news ch_2005

``` r
head(class_m_ids)
```

    ##     year address.country_code       file_id                             text
    ## 605 2005                   gb 050815-2.html                               uk
    ## 606 2005                   gb       434262a centre for ecology and hydrology
    ## 607 2005                   gb       438716a                           oxford
    ## 608 2005                   gb       435235b                    sigma-aldrich
    ## 609 2005                   gb       437456a                               uk
    ## 610 2005                   gb       435237a                               uk
    ##              ner    est_country est_un_region est_un_subregion
    ## 605      COUNTRY United Kingdom        Europe  Northern Europe
    ## 606 ORGANIZATION United Kingdom        Europe  Northern Europe
    ## 607 ORGANIZATION United Kingdom        Europe  Northern Europe
    ## 608 ORGANIZATION United Kingdom        Europe  Northern Europe
    ## 609      COUNTRY United Kingdom        Europe  Northern Europe
    ## 610      COUNTRY United Kingdom        Europe  Northern Europe
    ##                   type     idx
    ## 605               news gb_2005
    ## 606               news gb_2005
    ## 607               news gb_2005
    ## 608 technology-feature gb_2005
    ## 609               news gb_2005
    ## 610 technology-feature gb_2005

``` r
## filter for countries where they are mentioned more than once just for highest accuracy
class_c_ids_dups = data.frame(table(class_c_ids$file_id, class_c_ids$address.country_code))
class_c_ids_keep = subset(class_c_ids_dups, Freq > 1)
class_c_ids$freq_idx = paste(class_c_ids$file_id, class_c_ids$address.country_code, sep="_")
freq_pass = paste(class_c_ids_keep$Var1, class_c_ids_keep$Var2, sep="_")
class_c_ids = subset(class_c_ids, freq_idx %in% freq_pass)

class_m_ids_dups = data.frame(table(class_m_ids$file_id, class_m_ids$address.country_code))
class_m_ids_keep = subset(class_m_ids_dups, Freq > 1)
class_m_ids$freq_idx = paste(class_m_ids$file_id, class_m_ids$address.country_code, sep="_")
freq_pass = paste(class_m_ids_keep$Var1, class_m_ids_keep$Var2, sep="_")
class_m_ids = subset(class_m_ids, freq_idx %in% freq_pass)


# remove any files that are in the overlap
files_in_both = intersect(class_c_ids$file_id, class_m_ids$file_id)
class_c_ids = subset(class_c_ids, !file_id %in% files_in_both)
class_m_ids = subset(class_m_ids, !file_id %in% files_in_both)

head(class_c_ids)
```

    ##    year address.country_code        file_id                 text          ner
    ## 2  2005                   ch 051010-13.html          switzerland      COUNTRY
    ## 6  2005                   ch 050124-12.html world economic forum ORGANIZATION
    ## 10 2005                   ch  050509-7.html university of zurich ORGANIZATION
    ## 11 2005                   ch        436772a          switzerland      COUNTRY
    ## 12 2005                   ch  050509-7.html          switzerland      COUNTRY
    ## 13 2005                   ch  050822-7.html  university of basel ORGANIZATION
    ##    est_country est_un_region est_un_subregion         type     idx
    ## 2  Switzerland        Europe   Western Europe         news ch_2005
    ## 6  Switzerland        Europe   Western Europe         news ch_2005
    ## 10 Switzerland        Europe   Western Europe         news ch_2005
    ## 11 Switzerland        Europe   Western Europe news-feature ch_2005
    ## 12 Switzerland        Europe   Western Europe         news ch_2005
    ## 13 Switzerland        Europe   Western Europe         news ch_2005
    ##             freq_idx
    ## 2  051010-13.html_ch
    ## 6  050124-12.html_ch
    ## 10  050509-7.html_ch
    ## 11        436772a_ch
    ## 12  050509-7.html_ch
    ## 13  050822-7.html_ch

``` r
head(class_m_ids)
```

    ##      year address.country_code file_id                               text
    ## 1677 2005                   in 436168a                              india
    ## 1678 2005                   in 436168a indian space research organisation
    ## 1680 2005                   in 434259a        department of biotechnology
    ## 1682 2005                   in 436161a                              india
    ## 1683 2005                   in 433675a        jawaharlal nehru university
    ## 1685 2005                   in 436446a      bhabha atomic research center
    ##               ner est_country est_un_region est_un_subregion         type
    ## 1677      COUNTRY       India          Asia    Southern Asia news-feature
    ## 1678 ORGANIZATION       India          Asia    Southern Asia news-feature
    ## 1680 ORGANIZATION       India          Asia    Southern Asia         news
    ## 1682      COUNTRY       India          Asia    Southern Asia         news
    ## 1683 ORGANIZATION       India          Asia    Southern Asia         news
    ## 1685 ORGANIZATION       India          Asia    Southern Asia         news
    ##          idx   freq_idx
    ## 1677 in_2005 436168a_in
    ## 1678 in_2005 436168a_in
    ## 1680 in_2005 434259a_in
    ## 1682 in_2005 436161a_in
    ## 1683 in_2005 433675a_in
    ## 1685 in_2005 436446a_in

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
```

#### Calculate word frequencies for Class C

``` r
# get the word frequencies for each class of country
class_all_word_freq = get_word_freq_per_class(full_loc_df_pass, class_str = "class_all")

top_words_c = list()
# write out top words for each country
for(curr_country in unique(class_c_ids$est_country)){
    class_c_word_freq = get_word_freq_per_class(
                            subset(class_c_ids, est_country == curr_country), 
                            class_str = "class_c")
    per_class_word_freq = merge(data.table(class_c_word_freq), 
                                data.table(class_all_word_freq), by="word")
    
        
    # it should be more than 100 in at least one corpus
    per_class_word_freq = subset(per_class_word_freq, class_all_count > 100)
        
    per_class_word_freq$ratio = per_class_word_freq$class_c_count / 
                                per_class_word_freq$class_all_count
    
    per_class_word_freq = per_class_word_freq[order(per_class_word_freq$ratio, decreasing=T),]
    print(knitr::kable(head(per_class_word_freq,15), 
                 caption = paste(curr_country, "Class Citation, top terms")))
    top_words_c[[curr_country]] = per_class_word_freq$word[1:min(nrow(per_class_word_freq), 100)]

}
```

    ## 
    ## 
    ## Table: Switzerland Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |measles     |            37|             165| 0.2242424|
    ## |printing    |            39|             199| 0.1959799|
    ## |prions      |            28|             166| 0.1686747|
    ## |obese       |            16|             104| 0.1538462|
    ## |printed     |            25|             174| 0.1436782|
    ## |predatory   |            15|             109| 0.1376147|
    ## |facial      |            38|             285| 0.1333333|
    ## |drc         |            28|             219| 0.1278539|
    ## |lizards     |            13|             104| 0.1250000|
    ## |cov         |            55|             503| 0.1093439|
    ## |basel       |            15|             142| 0.1056338|
    ## |emotions    |            13|             137| 0.0948905|
    ## |expressions |            11|             116| 0.0948276|
    ## |spike       |            21|             228| 0.0921053|
    ## |radius      |            16|             175| 0.0914286|
    ## 
    ## 
    ## Table: Germany Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |leipzig     |            63|             133| 0.4736842|
    ## |neanderthal |           150|             371| 0.4043127|
    ## |munich      |           106|             289| 0.3667820|
    ## |heidelberg  |            64|             178| 0.3595506|
    ## |germanys    |           100|             279| 0.3584229|
    ## |tectonics   |            41|             115| 0.3565217|
    ## |gran        |            43|             123| 0.3495935|
    ## |refugees    |            57|             164| 0.3475610|
    ## |meyer       |            47|             140| 0.3357143|
    ## |confocal    |            34|             104| 0.3269231|
    ## |denisovans  |            44|             138| 0.3188406|
    ## |sasso       |            33|             104| 0.3173077|
    ## |reich       |            37|             122| 0.3032787|
    ## |max         |           236|             779| 0.3029525|
    ## |chimps      |            88|             296| 0.2972973|
    ## 
    ## 
    ## Table: France Class Citation, top terms
    ## 
    ## |word         | class_c_count| class_all_count|     ratio|
    ## |:------------|-------------:|---------------:|---------:|
    ## |cnrs         |            95|             186| 0.5107527|
    ## |thymus       |            53|             107| 0.4953271|
    ## |eth          |            41|             114| 0.3596491|
    ## |le           |            60|             167| 0.3592814|
    ## |ut           |            65|             185| 0.3513514|
    ## |frances      |            67|             209| 0.3205742|
    ## |purification |            40|             138| 0.2898551|
    ## |iter         |            75|             347| 0.2161383|
    ## |mrna         |            47|             228| 0.2061404|
    ## |wine         |            24|             117| 0.2051282|
    ## |philae       |            23|             117| 0.1965812|
    ## |french       |           170|            1019| 0.1668302|
    ## |fao          |            25|             151| 0.1655629|
    ## |hepatitis    |            35|             216| 0.1620370|
    ## |2d           |            36|             230| 0.1565217|
    ## 
    ## 
    ## Table: Japan Class Citation, top terms
    ## 
    ## |word       | class_c_count| class_all_count|     ratio|
    ## |:----------|-------------:|---------------:|---------:|
    ## |ut         |           109|             185| 0.5891892|
    ## |jaxa       |            59|             122| 0.4836066|
    ## |japans     |           130|             445| 0.2921348|
    ## |yamanaka   |            38|             138| 0.2753623|
    ## |tokyo      |           134|             508| 0.2637795|
    ## |sail       |            24|             101| 0.2376238|
    ## |japanese   |           184|             790| 0.2329114|
    ## |japan      |           397|            1990| 0.1994975|
    ## |riken      |            33|             179| 0.1843575|
    ## |antimatter |            48|             262| 0.1832061|
    ## |afm        |            19|             106| 0.1792453|
    ## |calorie    |            19|             114| 0.1666667|
    ## |tamiflu    |            18|             109| 0.1651376|
    ## |organoids  |            22|             135| 0.1629630|
    ## |confocal   |            16|             104| 0.1538462|
    ## 
    ## 
    ## Table: Netherlands Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |ut          |            88|             185| 0.4756757|
    ## |leiden      |            41|             130| 0.3153846|
    ## |netherlands |           202|            1002| 0.2015968|
    ## |biorxiv     |            29|             158| 0.1835443|
    ## |cats        |            50|             289| 0.1730104|
    ## |bison       |            18|             107| 0.1682243|
    ## |organoids   |            21|             135| 0.1555556|
    ## |oa          |            16|             124| 0.1290323|
    ## |motors      |            16|             126| 0.1269841|
    ## |citations   |            46|             378| 0.1216931|
    ## |dutch       |            39|             328| 0.1189024|
    ## |flip        |            12|             103| 0.1165049|
    ## |rankings    |            12|             104| 0.1153846|
    ## |switches    |            16|             151| 0.1059603|
    ## |organelles  |            15|             143| 0.1048951|
    ## 
    ## 
    ## Table: Canada Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |ut            |           139|             185| 0.7513514|
    ## |expressions   |            28|             116| 0.2413793|
    ## |clay          |            27|             138| 0.1956522|
    ## |dendritic     |            21|             112| 0.1875000|
    ## |athletes      |            24|             130| 0.1846154|
    ## |coronaviruses |            29|             158| 0.1835443|
    ## |mcgill        |            22|             121| 0.1818182|
    ## |emotions      |            24|             137| 0.1751825|
    ## |phage         |            18|             106| 0.1698113|
    ## |cambrian      |            20|             118| 0.1694915|
    ## |smallpox      |            17|             104| 0.1634615|
    ## |organoids     |            22|             135| 0.1629630|
    ## |vocal         |            22|             142| 0.1549296|
    ## |oa            |            19|             124| 0.1532258|
    ## |pd            |            24|             164| 0.1463415|
    ## 
    ## 
    ## Table: Belgium Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |song          |             7|             154| 0.0454545|
    ## |drain         |             4|             140| 0.0285714|
    ## |border        |             5|             297| 0.0168350|
    ## |era           |             7|             499| 0.0140281|
    ## |realizing     |             1|             101| 0.0099010|
    ## |bridges       |             1|             106| 0.0094340|
    ## |concedes      |             1|             107| 0.0093458|
    ## |mates         |             1|             108| 0.0092593|
    ## |cages         |             1|             114| 0.0087719|
    ## |strengthening |             1|             114| 0.0087719|
    ## |laid          |             3|             343| 0.0087464|
    ## |testosterone  |             1|             116| 0.0086207|
    ## |maternal      |             1|             117| 0.0085470|
    ## |performing    |             2|             234| 0.0085470|
    ## |inter         |             1|             124| 0.0080645|
    ## 
    ## 
    ## Table: Australia Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |csiro         |            38|             117| 0.3247863|
    ## |cannabis      |            32|             152| 0.2105263|
    ## |bison         |            18|             107| 0.1682243|
    ## |pig           |            37|             224| 0.1651786|
    ## |aus           |            17|             108| 0.1574074|
    ## |aboriginal    |            18|             121| 0.1487603|
    ## |bleaching     |            17|             115| 0.1478261|
    ## |australias    |            33|             233| 0.1416309|
    ## |marijuana     |            18|             132| 0.1363636|
    ## |mitochondrial |            65|             500| 0.1300000|
    ## |snakes        |            25|             194| 0.1288660|
    ## |riken         |            23|             179| 0.1284916|
    ## |lizards       |            13|             104| 0.1250000|
    ## |nio           |            32|             256| 0.1250000|
    ## |organoids     |            16|             135| 0.1185185|
    ## 
    ## 
    ## Table: Spain Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |neolithic   |            12|             105| 0.1142857|
    ## |spanish     |            20|             236| 0.0847458|
    ## |tooth       |            10|             125| 0.0800000|
    ## |proportions |             7|             107| 0.0654206|
    ## |reid        |             6|             104| 0.0576923|
    ## |passage     |            10|             177| 0.0564972|
    ## |ireland     |            12|             226| 0.0530973|
    ## |county      |             9|             173| 0.0520231|
    ## |burial      |             6|             124| 0.0483871|
    ## |spain       |            26|             543| 0.0478821|
    ## |hominins    |             6|             139| 0.0431655|
    ## |loans       |             4|             103| 0.0388350|
    ## |baker       |             5|             144| 0.0347222|
    ## |madrid      |             4|             120| 0.0333333|
    ## |hire        |             5|             152| 0.0328947|
    ## 
    ## 
    ## Table: Austria Class Citation, top terms
    ## 
    ## |word          | class_c_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |superposition |            15|             110| 0.1363636|
    ## |vienna        |            16|             238| 0.0672269|
    ## |gate          |             9|             141| 0.0638298|
    ## |qubit         |             8|             215| 0.0372093|
    ## |photon        |             9|             284| 0.0316901|
    ## |einstein      |             6|             241| 0.0248963|
    ## |interventions |             7|             291| 0.0240550|
    ## |epidemics     |             3|             125| 0.0240000|
    ## |measurement   |            11|             543| 0.0202578|
    ## |mechanics     |             8|             405| 0.0197531|
    ## |relativity    |             5|             262| 0.0190840|
    ## |lockdown      |             3|             160| 0.0187500|
    ## |polarization  |             3|             170| 0.0176471|
    ## |gates         |             9|             526| 0.0171103|
    ## |austria       |             3|             179| 0.0167598|
    ## 
    ## 
    ## Table: Sweden Class Citation, top terms
    ## 
    ## |word       | class_c_count| class_all_count|     ratio|
    ## |:----------|-------------:|---------------:|---------:|
    ## |tracing    |            38|             231| 0.1645022|
    ## |contacts   |            46|             331| 0.1389728|
    ## |quarantine |            14|             111| 0.1261261|
    ## |ethiopia   |            14|             171| 0.0818713|
    ## |lockdown   |            13|             160| 0.0812500|
    ## |irans      |             9|             117| 0.0769231|
    ## |dam        |            14|             209| 0.0669856|
    ## |contact    |            60|             934| 0.0642398|
    ## |karolinska |             7|             118| 0.0593220|
    ## |vietnam    |            12|             232| 0.0517241|
    ## |pcr        |            14|             276| 0.0507246|
    ## |filling    |             7|             150| 0.0466667|
    ## |sudan      |             6|             148| 0.0405405|
    ## |isolate    |             8|             202| 0.0396040|
    ## |apps       |             5|             127| 0.0393701|
    ## 
    ## 
    ## Table: Italy Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |volcanoes   |            26|             258| 0.1007752|
    ## |preprints   |            15|             154| 0.0974026|
    ## |volcano     |            31|             343| 0.0903790|
    ## |decode      |            13|             152| 0.0855263|
    ## |burial      |            10|             124| 0.0806452|
    ## |iceland     |            11|             148| 0.0743243|
    ## |lockdown    |            10|             160| 0.0625000|
    ## |milan       |             6|             103| 0.0582524|
    ## |appointment |             9|             181| 0.0497238|
    ## |chambers    |             7|             148| 0.0472973|
    ## |tourists    |             5|             106| 0.0471698|
    ## |walter      |             6|             135| 0.0444444|
    ## |usgs        |             8|             181| 0.0441989|
    ## |magma       |             9|             212| 0.0424528|
    ## |eruption    |            16|             381| 0.0419948|
    ## 
    ## 
    ## Table: Denmark Class Citation, top terms
    ## 
    ## |word        | class_c_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |masks       |            44|             152| 0.2894737|
    ## |mask        |            24|             112| 0.2142857|
    ## |smallpox    |            17|             104| 0.1634615|
    ## |danish      |            14|             114| 0.1228070|
    ## |respondents |            30|             407| 0.0737101|
    ## |postdocs    |            39|             572| 0.0681818|
    ## |denmark     |            18|             287| 0.0627178|
    ## |command     |             7|             131| 0.0534351|
    ## |surgical    |             8|             151| 0.0529801|
    ## |aerosols    |            13|             301| 0.0431894|
    ## |wearing     |             6|             140| 0.0428571|
    ## |satisfied   |             5|             130| 0.0384615|
    ## |fellowships |             4|             112| 0.0357143|
    ## |plague      |             6|             168| 0.0357143|
    ## |disorders   |            31|             917| 0.0338059|

``` r
top_words_c_freq = data.frame(sort(table(unlist(top_words_c)), decreasing=T))
top_words_c_freq$prop = top_words_c_freq$Freq / length(top_words_c)
print(knitr::kable(head(top_words_c_freq,15), 
                       caption = "Overall Class Mention, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms
    ## 
    ## |Var1        | Freq|      prop|
    ## |:-----------|----:|---------:|
    ## |cov         |    7| 0.5384615|
    ## |covid       |    6| 0.4615385|
    ## |lockdown    |    6| 0.4615385|
    ## |masks       |    5| 0.3846154|
    ## |sars        |    5| 0.3846154|
    ## |coronavirus |    4| 0.3076923|
    ## |distancing  |    4| 0.3076923|
    ## |hepatitis   |    4| 0.3076923|
    ## |newsblog    |    4| 0.3076923|
    ## |organoids   |    4| 0.3076923|
    ## |pandemic    |    4| 0.3076923|
    ## |ut          |    4| 0.3076923|
    ## |adverse     |    3| 0.2307692|
    ## |bc          |    3| 0.2307692|
    ## |biorxiv     |    3| 0.2307692|

#### Calculate word frequencies for Class M

``` r
# write out top words for each country
top_words_m = list()
for(curr_country in unique(class_m_ids$est_country)){
    class_m_word_freq = get_word_freq_per_class(
                            subset(class_m_ids, est_country == curr_country), 
                            class_str = "class_m")
    per_class_word_freq = merge(data.table(class_m_word_freq), 
                                data.table(class_all_word_freq), by="word")
    
        
    # it should be more than 100 in at least one corpus
    per_class_word_freq = subset(per_class_word_freq, class_all_count > 100)
        
    per_class_word_freq$ratio = per_class_word_freq$class_m_count / 
                                per_class_word_freq$class_all_count
    
    per_class_word_freq = per_class_word_freq[order(per_class_word_freq$ratio, decreasing=T),]
    print(knitr::kable(head(per_class_word_freq,15), 
                       caption = paste(curr_country, "Class Mention, top terms")))
    top_words_m[[curr_country]] = per_class_word_freq$word[1:min(nrow(per_class_word_freq), 100)]

}
```

    ## 
    ## 
    ## Table: India Class Mention, top terms
    ## 
    ## |word      | class_m_count| class_all_count|     ratio|
    ## |:---------|-------------:|---------------:|---------:|
    ## |bangalore |            77|             117| 0.6581197|
    ## |indias    |           359|             597| 0.6013400|
    ## |pachauri  |            71|             123| 0.5772358|
    ## |tiger     |           100|             203| 0.4926108|
    ## |delhi     |           129|             291| 0.4432990|
    ## |tigers    |            62|             145| 0.4275862|
    ## |singh     |            53|             132| 0.4015152|
    ## |indian    |           497|            1310| 0.3793893|
    ## |rao       |            39|             115| 0.3391304|
    ## |himalayas |            38|             119| 0.3193277|
    ## |bt        |            57|             181| 0.3149171|
    ## |india     |           660|            2132| 0.3095685|
    ## |cotton    |            59|             210| 0.2809524|
    ## |herbicide |            24|             104| 0.2307692|
    ## |monsanto  |            25|             115| 0.2173913|
    ## 
    ## 
    ## Table: Colombia Class Mention, top terms
    ## 
    ## |word          | class_m_count| class_all_count|     ratio|
    ## |:-------------|-------------:|---------------:|---------:|
    ## |weathering    |            14|             104| 0.1346154|
    ## |ccs           |            22|             174| 0.1264368|
    ## |nickel        |            14|             131| 0.1068702|
    ## |ppm           |            14|             135| 0.1037037|
    ## |petroleum     |            10|             117| 0.0854701|
    ## |trapping      |            13|             153| 0.0849673|
    ## |boron         |            11|             133| 0.0827068|
    ## |carbonate     |            20|             246| 0.0813008|
    ## |sequestration |            10|             130| 0.0769231|
    ## |mixtures      |             8|             109| 0.0733945|
    ## |erosion       |            22|             311| 0.0707395|
    ## |fatty         |            12|             175| 0.0685714|
    ## |ph            |            13|             190| 0.0684211|
    ## |elegans       |             7|             114| 0.0614035|
    ## |fuels         |            22|             440| 0.0500000|
    ## 
    ## 
    ## Table: Mexico Class Mention, top terms
    ## 
    ## |word         | class_m_count| class_all_count|     ratio|
    ## |:------------|-------------:|---------------:|---------:|
    ## |mexican      |            21|             185| 0.1135135|
    ## |monsanto     |            13|             115| 0.1130435|
    ## |tolerant     |             7|             106| 0.0660377|
    ## |humanitarian |             7|             108| 0.0648148|
    ## |spill        |            17|             312| 0.0544872|
    ## |maize        |            18|             380| 0.0473684|
    ## |chlorine     |             5|             106| 0.0471698|
    ## |golden       |            10|             222| 0.0450450|
    ## |fishermen    |             6|             144| 0.0416667|
    ## |mexico       |            42|            1160| 0.0362069|
    ## |pioneer      |             8|             255| 0.0313725|
    ## |drought      |            14|             492| 0.0284553|
    ## |transgenic   |             9|             353| 0.0254958|
    ## |citys        |             4|             161| 0.0248447|
    ## |projections  |             7|             285| 0.0245614|
    ## 
    ## 
    ## Table: Philippines Class Mention, top terms
    ## 
    ## |word        | class_m_count| class_all_count|     ratio|
    ## |:-----------|-------------:|---------------:|---------:|
    ## |peru        |             8|             139| 0.0575540|
    ## |rice        |            23|             635| 0.0362205|
    ## |philippines |             4|             150| 0.0266667|
    ## |golden      |             5|             222| 0.0225225|
    ## |vitamin     |             4|             208| 0.0192308|
    ## |sweet       |             2|             110| 0.0181818|
    ## |monday      |             2|             111| 0.0180180|
    ## |potato      |             2|             111| 0.0180180|
    ## |juan        |             2|             116| 0.0172414|
    ## |bangladesh  |             2|             149| 0.0134228|
    ## |poverty     |             4|             300| 0.0133333|
    ## |gdp         |             3|             230| 0.0130435|
    ## |coordinates |             2|             162| 0.0123457|
    ## |melinda     |             2|             164| 0.0121951|
    ## |cent        |             2|             191| 0.0104712|

``` r
top_words_m_freq = data.frame(sort(table(unlist(top_words_m)), decreasing=T))
top_words_m_freq$prop = top_words_m_freq$Freq / length(top_words_m)
print(knitr::kable(head(top_words_m_freq,15), 
                       caption = "Overall Class Mention, top terms"))
```

    ## 
    ## 
    ## Table: Overall Class Mention, top terms
    ## 
    ## |Var1       | Freq| prop|
    ## |:----------|----:|----:|
    ## |diesel     |    3| 0.75|
    ## |farmers    |    3| 0.75|
    ## |bangladesh |    2| 0.50|
    ## |catches    |    2| 0.50|
    ## |daughter   |    2| 0.50|
    ## |farming    |    2| 0.50|
    ## |gm         |    2| 0.50|
    ## |golden     |    2| 0.50|
    ## |harvest    |    2| 0.50|
    ## |maize      |    2| 0.50|
    ## |melinda    |    2| 0.50|
    ## |missouri   |    2| 0.50|
    ## |monsanto   |    2| 0.50|
    ## |pesticide  |    2| 0.50|
    ## |pesticides |    2| 0.50|
