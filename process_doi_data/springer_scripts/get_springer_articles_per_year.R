require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))


#' format query into Springer-appropriate URL query
#' 
#' @param country, country where at least one author has an affiliation
#' @param year, year the journal publication was made
#' @param api_key, personal API_KEY
#' @return string, url
url_springer_year_search <- function(year, api_key) {

    # load libraries
    require(RCurl)
    options(useFancyQuotes = FALSE)


    # nominatim search api url
    url_springer_search_api <- "http://api.springernature.com/metadata/json?q=(type:Journal AND pub:Nature AND language:en AND "
    query_str <- paste(url_springer_search_api, 
                        "year:", year, ")",
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
single_springer_year_query <- function(curr_year, api_key){

    # we need to follow Springer quidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    cache_file = file.path(ref_data_dir, "/springer_year_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_year_cache file not found, will be created")
        cache_df = NA
    }else{
        cache_df = data.frame(fread(cache_file))
        query_df = subset(cache_df, year == curr_year)
        if(nrow(query_df) != 0){
            return(query_df)
        }
    }


    #run query
    print(paste("running query:", curr_year))
    query_url = url_springer_year_search(curr_year, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = data.frame("year" = curr_year, "num_entries" = NA)
    if(all(!is.na(resp))){
        num_entries = as.numeric(resp$result$total)
        resp_df$num_entries = num_entries
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
    cache_file = file.path(ref_data_dir, "/springer_year_cache.tsv")
    write.table(cache_df, cache_file, sep="\t", quote=F, row.names=F)

    # sleep because we are limited in API requests per second
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
batch_springer_year_query <- function(year_vec, api_key){


    # query one at a time and append together
    batch_resp = NA
    for(curr_year in year_vec){
            curr_resp = single_springer_year_query(curr_year, api_key)
            batch_resp = rbind(batch_resp, curr_resp)
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
initialize_springer_total_query <- function(api_key){

    # query each  year
    year_vec = 2005:2020
    batch_resp = batch_springer_year_query(year_vec, api_key)

    return(batch_resp)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
initialize_springer_total_query(api_key)
