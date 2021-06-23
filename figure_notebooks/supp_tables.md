Supp\_tables
================
Natalie Davidson
6/15/2021

## Overview

This notebook generates supplemental tables 1-4

The **data** it uses to build the tables are here:

1.  nature news scraped data after coreNLP: `/data/scraped_data/downloads/coreNLP_output*`

2.  nature news quotes: `/data/scraped_data/downloads/quote_table_raw*`

3.  quoted speakers and name origin prediction: `/data/author_data/all_speaker_fullname_pred.tsv` `/data/author_data/all_speaker_fullname.tsv`

4.  springer authors: `/data/reference_data/springer_bg_author_cache.tsv`

5.  Nature News cited authors in a springer journal: `/data/reference_data/springer_cited_author_cache.tsv`

6.  All cited DOIs: `data/doi_data/downloads/*`

7.  All authors' names and name origin predictions: `/data/author_data/all_author_fullname_pred.tsv`

8.  Nature and Springer Authors' gender predictions: `/data/author_data/nature_author_gender.tsv` `/data/author_data/springer_author_gender.tsv`

The **setting + helper functions** to generate the plots are here:

1.  plotting related functions: `/utils/plotting_utils.R`

2.  reading + data processing related functions: `/utils/scraper_processing_utils.R`

3.  nautre research article and springer specific data processing functions: `/process_doi_data/springer_scripts/springer_scrape_utils.R`

## Read in the data

### Read in reference data

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

    ##   year type        file_id
    ## 2 2005 news  041220-1.html
    ## 3 2005 news  050103-1.html
    ## 4 2005 news 050103-10.html
    ## 5 2005 news 050103-11.html
    ## 6 2005 news 050103-12.html
    ## 7 2005 news  050103-2.html

### Read in the quotes

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


# read in the name prediction information for quotes
name_pred_file = file.path(proj_dir, 
                             "/data/author_data/all_speaker_fullname_pred.tsv")
name_info_file = file.path(proj_dir, 
                             "/data/author_data/all_speaker_fullname.tsv")
quote_name_origin_df = read_name_origin(name_pred_file, name_info_file)
quote_name_origin_df = subset(quote_name_origin_df, !type %in% c("career-column", "news-and-views"))
```

### Read in the authorship info (springer, nature, and cited articles)

``` r
# we have 3 source files for author info
# springer background authorship
springer_author_file = file.path(proj_dir, 
                                "/data/reference_data/springer_bg_author_cache.tsv")
springer_author_df = data.frame(fread(springer_author_file))
springer_author_df$file_id = NA

# springer cited authorship
cited_author_file = file.path(proj_dir, 
                                "/data/reference_data/springer_cited_author_cache.tsv")
cited_dois_dir = file.path(proj_dir, "data/doi_data/downloads")

cited_author_df = data.frame(fread(cited_author_file))
cited_author_df = subset(cited_author_df, !is.na(authors))
cited_author_df$pub_year = cited_author_df$year
cited_author_df = subset(cited_author_df, select=-c(year))
cited_doi_df = get_ref_dois(cited_dois_dir)
```

    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2005_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2006_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2007_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2008_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2009_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2010_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2011_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2012_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2013_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2014_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2015_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2016_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2017_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2018_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2019_toolbox.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_career-column.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_career-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_news-and-views.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_news-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_news.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_technology-feature.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/links_crawled_2020_toolbox.json"

``` r
cited_author_df = merge(cited_author_df, cited_doi_df[,c("doi", "year", "file_id")], by=c("doi"))

# then all the nature articles
nature_dir = file.path(proj_dir, "data/author_data/downloads")
nature_author_df = read_nature_author_json_files(nature_dir)
```

    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2005_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2005_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2006_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2006_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2007_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2007_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2008_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2008_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2009_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2009_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2010_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2010_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2011_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2011_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2012_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2012_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2013_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2013_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2014_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2014_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2015_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2015_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2016_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2016_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2017_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2017_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2018_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2018_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2019_article.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2019_letter.json"
    ## [1] "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads/links_crawled_2020_article.json"

``` r
# now read in the processed data for name prediction
name_pred_file = file.path(proj_dir, 
                         "/data/author_data/all_author_fullname_pred.tsv")
name_info_file = file.path(proj_dir, 
                         "/data/author_data/all_author_fullname.tsv")
origin_cite_name_df = read_name_origin(name_pred_file, name_info_file)
origin_cite_name_df$name_origin[origin_cite_name_df$name_origin == "Jewish"] = "Hebrew"


# seperate out citations from columns by journalists vs scientists
journalist_idx = which(origin_cite_name_df$corpus == "naturenews_citations" &
                         origin_cite_name_df$file_id %in% news_df$file_id)
scientist_idx = which(origin_cite_name_df$corpus == "naturenews_citations" &
                         !origin_cite_name_df$file_id %in% news_df$file_id)
origin_cite_name_df$corpus[journalist_idx] = "citation_journalist"
origin_cite_name_df$corpus[scientist_idx] = "citation_scientist"

# now read in the processed data for gender prediction
springer_gender_file = file.path(proj_dir, 
                         "/data/author_data/springer_author_gender.tsv")
springer_pred_gender_df = data.frame(fread(springer_gender_file))

nature_file = file.path(proj_dir, "/data/author_data/nature_author_gender.tsv")
nature_pred_gender_df = read_gender_files(nature_file)
head(nature_pred_gender_df)
```

    ##       author                            doi year author_pos            file_id
    ## 1     aakash doi:10.1038/s41586-020-03052-3 2020      first s41586-020-03052-3
    ## 2 aanindeeta        doi:10.1038/nature17185 2016      first        nature17185
    ## 3      aaron  doi:10.1038/s41586-020-3009-y 2020      first  s41586-020-3009-y
    ## 4      aaron        doi:10.1038/nature07885 2009       last        nature07885
    ## 5      aaron        doi:10.1038/nature03831 2005       last        nature03831
    ## 6      aaron        doi:10.1038/nature04790 2006       last        nature04790
    ##   est_gender gender
    ## 1       MALE   MALE
    ## 2       <NA>   <NA>
    ## 3       MALE   MALE
    ## 4       MALE   MALE
    ## 5       MALE   MALE
    ## 6       MALE   MALE

## Get Table info

### Gender prediction for quotes

``` r
# filter out career column and news-and-views
full_quote_df_gender = subset(full_quote_df, !type %in% c("career-column", "news-and-views"))
full_quote_df_gender = unique(full_quote_df_gender)

print("Total Quotes")
```

    ## [1] "Total Quotes"

``` r
total_quotes = nrow(full_quote_df_gender)
print(total_quotes)
```

    ## [1] 119998

``` r
# remove names with single name and do not have a pronoun
space_idx = grep(" ", full_quote_df_gender$est_speaker)
gendered_pronouns = c("he", "him", "his", "himself",
                    "she", "her", "hers", "herself")
pronoun_idx_canonical = which(full_quote_df_gender$canonical_speaker %in% gendered_pronouns)
pronoun_idx_partial = which(full_quote_df_gender$partial_name %in% gendered_pronouns)
allowed_idx = unique(c(space_idx, pronoun_idx_canonical, pronoun_idx_partial))
full_quote_df_gender = full_quote_df_gender[allowed_idx,]
full_quote_df_gender = unique(full_quote_df_gender)

print("Total quotes with full name or pronoun")
```

    ## [1] "Total quotes with full name or pronoun"

``` r
quotes_with_name_or_pronoun = nrow(full_quote_df_gender)
print(quotes_with_name_or_pronoun)
```

    ## [1] 110035

``` r
# remove quotes where no gender could be estimated
full_quote_df_gender = subset(full_quote_df_gender, !is.na(est_gender))
full_quote_df_gender = full_quote_df_gender[full_quote_df_gender$est_gender %in% c("FEMALE", "MALE"), ]

print("Total quotes with Gender Prediction")
```

    ## [1] "Total quotes with Gender Prediction"

``` r
full_quote_df_gender = unique(full_quote_df_gender)
quotes_with_gender = nrow(full_quote_df_gender)
print(quotes_with_gender)
```

    ## [1] 109723

### Name origin prediction for quotes

``` r
# filter out career column and news-and-views
full_quote_df_origin = subset(full_quote_df, !type %in% c("career-column", "news-and-views"))
full_quote_df_origin = unique(full_quote_df_origin)


# first we remove anything that is not a full name
# i.e. there must be a space
space_name_idx = grep(" ", full_quote_df_origin$est_speaker)
full_quote_df_origin = full_quote_df_origin[space_name_idx, ]

# now format the author name
full_quote_df_origin$author = format_author_fullname(full_quote_df_origin$est_speaker)

# format the output
col_ids = c("year", "type", "author", "file_id", "quote")
full_quote_df_origin = unique(full_quote_df_origin[,col_ids])

print("Total quotes with full name")
```

    ## [1] "Total quotes with full name"

``` r
quotes_with_fullname = nrow(full_quote_df_origin)
print(quotes_with_fullname)
```

    ## [1] 100529

``` r
print("Total quotes with name origin prediction")
```

    ## [1] "Total quotes with name origin prediction"

``` r
quotes_with_name_pred = nrow(unique(quote_name_origin_df))
print(quotes_with_name_pred)
```

    ## [1] 100528

``` r
table1_res = c(total_quotes, quotes_with_name_or_pronoun, quotes_with_gender, quotes_with_fullname, quotes_with_name_pred)
names(table1_res) = c("total_quotes", 
                      "quotes_with_name_or_pronoun", 
                      "quotes_with_gender_pred",
                      "quotes_with_fullname",
                      "quotes_with_name_pred")
```

### Name Origin prediction for citations

``` r
# first get all citations accessed
total_cite_name_df = cited_doi_df
journalist_idx = which(total_cite_name_df$file_id %in% news_df$file_id)
scientist_idx = which(!total_cite_name_df$file_id %in% news_df$file_id)
total_cite_name_df$corpus = ""
total_cite_name_df$corpus[journalist_idx] = "citation_journalist"
total_cite_name_df$corpus[scientist_idx] = "citation_scientist"

print("Number of Springer Citations")
```

    ## [1] "Number of Springer Citations"

``` r
total_cite_name_df = unique(total_cite_name_df)
tot_citations = data.frame(table(total_cite_name_df$corpus))
colnames(tot_citations) = c("type", "total_citations")
print(tot_citations)
```

    ##                  type total_citations
    ## 1 citation_journalist           15713
    ## 2  citation_scientist           40707

``` r
# now get all citations accessed in springer
cite_name_df = cited_author_df

# seperate out citations from columns by journalists vs scientists
journalist_idx = which(cite_name_df$file_id %in% news_df$file_id)
scientist_idx = which(!cite_name_df$file_id %in% news_df$file_id)
cite_name_df$corpus = ""
cite_name_df$corpus[journalist_idx] = "citation_journalist"
cite_name_df$corpus[scientist_idx] = "citation_scientist"

print("Number of Springer Citations")
```

    ## [1] "Number of Springer Citations"

``` r
cite_name_df = unique(cite_name_df)
springer_citations = data.frame(table(cite_name_df$corpus))
colnames(springer_citations) = c("type", "total_springer_citations")
print(springer_citations)
```

    ##                  type total_springer_citations
    ## 1 citation_journalist                     5736
    ## 2  citation_scientist                    14597

``` r
# format the author names and add the correct corpus
cited_author_df_full = format_authors(cite_name_df, use_fullname=T)
cited_author_df_full = subset(cited_author_df_full, author != "")
journalist_idx = which(cited_author_df_full$file_id %in% news_df$file_id)
scientist_idx = which(!cited_author_df_full$file_id %in% news_df$file_id)
cited_author_df_full$corpus = ""
cited_author_df_full$corpus[journalist_idx] = "citation_journalist"
cited_author_df_full$corpus[scientist_idx] = "citation_scientist"

print("Number of Citations with full name")
```

    ## [1] "Number of Citations with full name"

``` r
cited_author_df_full = unique(cited_author_df_full)
named_citations = data.frame(table(cited_author_df_full$corpus, cited_author_df_full$author_pos))
named_citations = dcast(Var1 ~ Var2, data = named_citations, value.var="Freq")
colnames(named_citations) = c("type", "fullname_first_author_citations", "fullname_last_author_citations")
print(named_citations)
```

    ##                  type fullname_first_author_citations
    ## 1 citation_journalist                            4405
    ## 2  citation_scientist                           11151
    ##   fullname_last_author_citations
    ## 1                           4423
    ## 2                          11083

``` r
# filter to the authors with a name prediciton
pred_cite = subset(origin_cite_name_df, corpus %in% c("citation_journalist", "citation_scientist"))
print("Number of Citations with prediction")
```

    ## [1] "Number of Citations with prediction"

``` r
pred_cite = unique(pred_cite)
pred_citations = data.frame(table(pred_cite$corpus, pred_cite$author_pos))
pred_citations = dcast(Var1 ~ Var2, data = pred_citations, value.var="Freq")
colnames(pred_citations) = c("type", 
                              "name_origin_pred_first_author_citations", 
                              "name_origin_pred_last_author_citations")
print(pred_citations)
```

    ##                  type name_origin_pred_first_author_citations
    ## 1 citation_journalist                                    4402
    ## 2  citation_scientist                                   11151
    ##   name_origin_pred_last_author_citations
    ## 1                                   4406
    ## 2                                  11065

``` r
table2_res = Reduce(merge, list(tot_citations, springer_citations, 
                                named_citations, pred_citations ))
```

### Gender + origin prediction for Springer

``` r
springer_gender_df = springer_author_df
print("Number of Total Springer")
```

    ## [1] "Number of Total Springer"

``` r
springer_gender_df = unique(springer_gender_df)
num_citations_springer = nrow(springer_gender_df)
print(num_citations_springer)
```

    ## [1] 38400

``` r
springer_gender_df = format_authors(springer_gender_df)
springer_gender_df = subset(springer_gender_df, author != "")
print("Number of Springer Full name")
```

    ## [1] "Number of Springer Full name"

``` r
springer_gender_df = unique(springer_gender_df)
num_names_springer = nrow(springer_gender_df)
print(num_names_springer)
```

    ## [1] 54509

``` r
res = get_author_gender(springer_gender_df)
springer_author_df_gender = res[[2]]
print("Number of Springer Full name")
```

    ## [1] "Number of Springer Full name"

``` r
springer_author_df_gender = subset(springer_author_df_gender, 
                                   !is.na(guessed_gender))
springer_author_df_gender = unique(springer_author_df_gender)
num_gender_springer = nrow(springer_author_df_gender)
print("Number of Springer Gender Prediction")
```

    ## [1] "Number of Springer Gender Prediction"

``` r
print(num_gender_springer)
```

    ## [1] 50877

``` r
# double check that it matches the pre-processed table we are working with
springer_pred_gender_df_pass = subset(springer_pred_gender_df, !is.na(guessed_gender))
springer_pred_gender_df_pass = unique(springer_pred_gender_df_pass)
num_names_springer_preprocessed = nrow(springer_pred_gender_df_pass)
print(num_names_springer_preprocessed)
```

    ## [1] 50877

``` r
print(num_names_springer_preprocessed == num_gender_springer)
```

    ## [1] TRUE

``` r
pred_springer = subset(origin_cite_name_df, corpus  == "springer_articles")
print("Number of Springer with prediction")
```

    ## [1] "Number of Springer with prediction"

``` r
pred_springer = unique(pred_springer)
pred_springer = nrow(pred_springer)
print(pred_springer)
```

    ## [1] 54358

``` r
table3_res = c(num_citations_springer, num_names_springer, num_gender_springer, pred_springer)
names(table3_res) = c("num_springer_articles", "num_springer_full_names", "num_springer_gender_pred", "num_springer_origin_pred")
```

### Gender + origin prediction for Nature

``` r
nature_gender_df = nature_author_df
print("Number of Total Nature")
```

    ## [1] "Number of Total Nature"

``` r
nature_gender_df = unique(nature_gender_df)
num_citations_nature = nrow(nature_gender_df)
print(num_citations_nature)
```

    ## [1] 13414

``` r
nature_gender_df = format_authors(nature_gender_df)
nature_gender_df = subset(nature_gender_df, author != "")
print("Number of Nature Full name")
```

    ## [1] "Number of Nature Full name"

``` r
nature_gender_df = unique(nature_gender_df)
num_names_nature = nrow(nature_gender_df)
print(num_names_nature)
```

    ## [1] 21765

``` r
res = get_author_gender(nature_gender_df)
nature_author_df_gender = res[[2]]
print("Number of nature Full name")
```

    ## [1] "Number of nature Full name"

``` r
nature_author_df_gender = subset(nature_author_df_gender, 
                                   !is.na(guessed_gender))

nature_author_df_gender = unique(nature_author_df_gender)
num_gender_nature = nrow(nature_author_df_gender)
print("Number of nature Gender Prediction")
```

    ## [1] "Number of nature Gender Prediction"

``` r
print(num_gender_nature)
```

    ## [1] 20942

``` r
# double check that it matches the pre-processed table we are working with
nature_pred_gender_df_pass = subset(nature_pred_gender_df, !is.na(est_gender))
num_names_nature_preprocessed = nrow(nature_pred_gender_df_pass)
print(num_names_nature_preprocessed)
```

    ## [1] 20942

``` r
print(num_names_nature_preprocessed == num_gender_nature)
```

    ## [1] TRUE

``` r
origin_cite_name_df = unique(origin_cite_name_df)
pred_nature = subset(origin_cite_name_df, corpus  == "nature_articles")
print("Number of nature with prediction")
```

    ## [1] "Number of nature with prediction"

``` r
pred_nature = nrow(pred_nature)
print(pred_nature)
```

    ## [1] 21765

``` r
table4_res = c(num_citations_nature, num_names_nature, num_gender_nature, pred_nature)
names(table4_res) = c("num_nature_articles", "num_nature_full_names", "num_nature_gender_pred", "num_nature_origin_pred")
```

### Make Tables

``` r
knitr::kable(data.frame(Frequency=table1_res), format = "pipe", 
             caption = "Breakdown of quotes at major processing steps")
```

|                                 |  Frequency|
|:--------------------------------|----------:|
| total\_quotes                   |     119998|
| quotes\_with\_name\_or\_pronoun |     110035|
| quotes\_with\_gender\_pred      |     109723|
| quotes\_with\_fullname          |     100529|
| quotes\_with\_name\_pred        |     100528|

``` r
knitr::kable(table2_res, format = "pipe", 
             caption = "Breakdown of citations at major processing steps")
```

<table style="width:100%;">
<caption>Breakdown of citations at major processing steps</caption>
<colgroup>
<col width="10%" />
<col width="8%" />
<col width="12%" />
<col width="15%" />
<col width="15%" />
<col width="19%" />
<col width="19%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">type</th>
<th align="right">total_citations</th>
<th align="right">total_springer_citations</th>
<th align="right">fullname_first_author_citations</th>
<th align="right">fullname_last_author_citations</th>
<th align="right">name_origin_pred_first_author_citations</th>
<th align="right">name_origin_pred_last_author_citations</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">citation_journalist</td>
<td align="right">15713</td>
<td align="right">5736</td>
<td align="right">4405</td>
<td align="right">4423</td>
<td align="right">4402</td>
<td align="right">4406</td>
</tr>
<tr class="even">
<td align="left">citation_scientist</td>
<td align="right">40707</td>
<td align="right">14597</td>
<td align="right">11151</td>
<td align="right">11083</td>
<td align="right">11151</td>
<td align="right">11065</td>
</tr>
</tbody>
</table>

``` r
knitr::kable(data.frame(Frequency=table3_res), format = "pipe", 
             caption = "Breakdown of all Springer articles at major processing steps")
```

|                             |  Frequency|
|:----------------------------|----------:|
| num\_springer\_articles     |      38400|
| num\_springer\_full\_names  |      54509|
| num\_springer\_gender\_pred |      50877|
| num\_springer\_origin\_pred |      54358|

``` r
knitr::kable(data.frame(Frequency=table4_res), format = "pipe", 
             caption = "Breakdown of all Nature articles at major processing steps")
```

|                           |  Frequency|
|:--------------------------|----------:|
| num\_nature\_articles     |      13414|
| num\_nature\_full\_names  |      21765|
| num\_nature\_gender\_pred |      20942|
| num\_nature\_origin\_pred |      21765|
