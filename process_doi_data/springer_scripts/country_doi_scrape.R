require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))

## initialize reading in data frame for use in querying
springer_country_file = file.path(ref_data_dir, "springer_country_map.tsv")
if(!file.exists(springer_country_file)){
        stop("springer name translations not found, 
            this should be in the GIT repo named springer_country_map.tsv 
            in data/reference_data")
}
springer_country_map = data.frame(fread(springer_country_file))

#' format query into Springer-appropriate URL query
#' 
#' @param country, country where at least one author has an affiliation
#' @param year, year the journal publication was made
#' @param api_key, personal API_KEY
#' @return string, url
url_springer_country_search <- function(country, year, api_key) {

    # load libraries
    require(RCurl)
    options(useFancyQuotes = FALSE)

    # check if country is not in springer format
    if(country %in% springer_country_map$cdh_country){
        country = springer_country_map$springer_country[
                        which(springer_country_map$cdh_country == country)
                        ]
    }

    # nominatim search api url
    url_springer_search_api <- "http://api.springernature.com/metadata/json?q=(type:Journal AND "
    query_str <- paste(url_springer_search_api, 
                        "year:", year,
                        " AND country:", dQuote(country), ")",
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
single_springer_country_query <- function(curr_country, curr_year, api_key){

    # we need to follow Springer quidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    cache_file = file.path(ref_data_dir, "/springer_country_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_country_cache file not found, will be created")
        cache_df = NA
    }else{
        cache_df = data.frame(fread(cache_file))
        query_df = subset(cache_df, country == curr_country & year == curr_year)
        if(nrow(query_df) != 0){
            return(query_df)
        }
    }


    #run query
    print(paste("running query:", curr_country, curr_year))
    query_url = url_springer_country_search(curr_country, curr_year, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = data.frame("country" = curr_country,
                        "year" = curr_year, "num_entries" = NA)
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
    cache_file = file.path(ref_data_dir, "/springer_country_cache.tsv")
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
batch_springer_country_query <- function(year_vec, country_vec, api_key){


    # query one at a time and append together
    batch_resp = NA
    for(curr_year in year_vec){
        for(curr_country in country_vec){
            curr_resp = single_springer_country_query(curr_country, curr_year, api_key)
            batch_resp = rbind(batch_resp, curr_resp)
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
initialize_springer_country_query <- function(api_key){

    # run initial springer country query
    country_info = get_country_info()

    # query each country for each year
    year_vec = 2012:2020
    country_vec = unique(country_info$country)
    batch_resp = batch_springer_country_query(year_vec, country_vec, api_key)

    return(batch_resp)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
initialize_springer_country_query(api_key)
