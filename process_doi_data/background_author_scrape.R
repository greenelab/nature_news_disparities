require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scrape_utils.R"))


#' format query into Springer-appropriate URL query
#' 
#' @param country, country where at least one author has an affiliation
#' @param year, year the journal publication was made
#' @param api_key, personal API_KEY
#' @return string, url
url_springer_bg_author_search <- function(year, month, page_start, api_key) {

    # load libraries
    require(RCurl)

    # write date string with wild card
    if(month < 10){
        month = paste("0", month, sep="")
    }
    date_str = paste(year, month, "*", sep="-")

    # nominatim search api url
    url_springer_search_api <- "http://api.springernature.com/metadata/json?q=(type:Journal AND "
    query_str <- paste(url_springer_search_api, 
                        "onlinedate:", date_str, ")",
                        "&p=50",
                        "&s=", (page_start-1)*50+1,
                        "&api_key=", api_key,
                        sep="")


    # percent-encode search request
    single_query <- URLencode(query_str)
    
    return(single_query)
}



#' public method that makes a single query to Springer.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' 
#' @param country, country where at least one author has an affiliation
#' @param year, year the journal publication was made
#' @param api_key, personal API_KEY
#' @return dataframe result from OSM with formatted location data
single_springer_bg_author_query <- function(curr_year, curr_month, curr_page, api_key){

    # we need to follow Springer quidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    cache_file = file.path(ref_data_dir, "/springer_bg_author_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_bg_author_cache file not found, will be created")
        cache_df = NA
    }else{
        cache_df = data.frame(fread(cache_file))
        query_df = subset(cache_df, month == curr_month & 
                            year == curr_year & 
                            page == curr_page)
        if(nrow(query_df) != 0){
            return(query_df)
        }
    }


    #run query
    print(paste("running query:", curr_year, curr_month, curr_page))
    query_url = url_springer_bg_author_search(curr_year, curr_month, curr_page, api_key)
    resp = internal_springer_query(query_url)


    # format response
    resp_df = NA
    if(!all(is.na(resp)) & resp$result$total > 0){
        na_elem = rep(NA, as.numeric(resp$result$recordsDisplayed))
        resp_df = data.frame( "month" = curr_month, "year" = curr_year,
                        "page" = curr_page,
                        "doi" = na_elem,
                        "content_type" = na_elem,
                        "article_type" = na_elem,
                        "authors" = na_elem)


        resp_df$doi = paste("doi:", resp$records$doi, sep="")
        resp_df$content_type = resp$records$contentType

        resp_df$article_type = unlist(lapply(resp$records$genre,
                                    function(x) paste(x, collapse="; ")))
        resp_df$authors = unlist(lapply(resp$records$creators,
                                function(x) 
                                    paste(x$creator, collapse="; ")))
    }




    # get metadata
    query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    resp_df$query_date = query_date    
    
    # append to the cache
    if(!all(is.na(cache_df))){
        # format the cache data frame
        resp_df = resp_df[,colnames(cache_df)]
        cache_df = rbind(cache_df, resp_df)
    }else{
        cache_df = resp_df
    }
    cache_file = file.path(ref_data_dir, "/springer_bg_author_cache.tsv")
    write.table(cache_df, cache_file, sep="\t", quote=F, row.names=F)

    Sys.sleep(1)

    return(resp_df)


}

#' public method that makes multiple queries to Springer API.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' 
#' @param year_vec, vector of years to query over
#' @param country_vec, vector of countries to query over
#' @param api_key, personal API_KEY
#' @return dataframe result from Springer
batch_springer_bg_author_query <- function(year_vec, month_vec, page_vec, api_key){


    # query one at a time and append together
    batch_resp = NA
    for(curr_year in year_vec){
        for(curr_month in month_vec){
            for(curr_page in page_vec){
                curr_resp = single_springer_bg_author_query(curr_year, curr_month, curr_page, api_key)
                batch_resp = rbind(batch_resp, curr_resp)
            }
        }
    }
    batch_resp = batch_resp[-1,]
    return(batch_resp)

}

#' public method that updates/fills the country+year cache. 
#' This method must be run by users the first time, 
#' since I am unable to share my mined data
#' 
#' @param api_key, API Key for Springer API, the user must make their own
#' this can be attained through registration here: 
#' https://dev.springernature.com/signup?cannot_be_converted_to_param
initialize_springer_bg_author_query <- function(api_key){

    # query each month for each year
    year_vec = 2005:2020
    month_vec = 1:12
    page_vec = 1:4

    batch_resp = batch_springer_bg_author_query(year_vec, month_vec, api_key)

    return(batch_resp)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
initialize_springer_bg_author_query(api_key)
