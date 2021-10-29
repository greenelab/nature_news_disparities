require(tidyr)
require(jsonlite)
require(data.table)
require(textclean)
require(stringr)

require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))



#' private internal method that queries guardian
#' 
#' @param string; URL formatted for guardian API
#' @return OSM JSON response
internal_guardian_query <- function(url_search){

    # block augmented from here: 
    # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    out <- tryCatch(
        {
            #resp = getURL(url_search, .encoding='UTF-8')
            #resp = gsub("\u0001|\u0003|\u001a|\u0002|\u001d|<p>", "", resp)
            #resp = gsub("’", "'", resp)
            jsonlite::fromJSON(url_search)
        },
        error=function(cond) {
            message(paste("query error:", url_search))
            # Choose a return value in case of error
            return(NA)
        }
    )    
    return(out)
}

#' format query into Guardian-appropriate URL query
#' intended to get a max of 100 articles per month
#' 
#' @param doi_chunk, vector of dois
#' @param api_key, personal API_KEY
#' @return string, url for Guardian API call
url_guardian <- function(from_date, to_date, api_key) {

    # load libraries
    library(RCurl)


    # search api url
    url_guardian_search_api <- "https://content.guardianapis.com/search?section=science"
    query_str <- paste(url_guardian_search_api, 
                        "&from-date=", from_date,
                        "&to-date=", to_date,
                        "&show-fields=body&page-size=100&",
                        "api-key=", api_key,
                        sep="")


    # percent-encode search request
    single_query <- URLencode(query_str, repeated=T)
    
    return(single_query)
}


#' public method that makes a single query to Guardian.
#' This method takes into account all Guardian query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Guardian and 
#' update the cache
#' 
#' @param year, year the news article was published
#' @param month, month the news article was published
#' @param api_key, personal API_KEY
#' @return dataframe result from Guardian
single_guardian_query <- function(curr_year, curr_month, api_key){

    # we need to follow Guardian quidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 12 queries per second


    #run query
    print(paste("running query:", curr_year, curr_month))
    from_date = paste0(curr_year, "-", curr_month)
    to_date = paste0(curr_year, "-", curr_month+1)
    if(curr_month == 12){
        to_date = paste0(curr_year+1, "-", 1)
    }
    query_url = url_guardian(from_date, to_date, api_key)
    resp = internal_guardian_query(query_url)
    resp_res = resp$response$results


    # format response
    #resp_res = lapply(resp_res, unlist)
    #resp_res = data.frame(t(data.frame(resp_res)))
    resp_res$body = replace_html(resp_res$fields$body)
    
    # remove redundant white space
    resp_res$body = gsub("\n", "", resp_res$body)
    resp_res$body = str_squish(resp_res$body)

    # convert unicode stuff
    ## TODO: Do this for all languages, JIC
    resp_res$body = gsub("\u2019|\u2018|‘|’", "'", resp_res$body)
    resp_res$body = gsub("\u201c|\u201d|“|”|\u0022", "\"", resp_res$body)
    resp_res$body = iconv(resp_res$body, from = "UTF-8", to = "ASCII", sub = "")


    resp_res = resp_res[,c("id", "body")]
    resp_res$year = curr_year
    resp_res = resp_res[,c("id", "year", "body")]
    colnames(resp_res)[1] = "file_id"

    #file id will be a filename later so make it a proper name
    resp_res$file_id = make.names(resp_res$file_id)

    Sys.sleep(1)

    return(resp_res)


}

#' public method that makes multiple queries to Springer API.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' 
#' @param year_vec, vector of years to query over
#' @param month_vec, vector of months to query over
#' @param api_key, personal API_KEY
#' @return dataframe result from Guardian
batch_guardian_query <- function(year_vec, month_vec, api_key){

    # query one at a time and append together
    for(curr_year in year_vec){
        batch_resp = NA
        for(curr_month in month_vec){
            curr_resp = single_guardian_query(curr_year, curr_month, api_key)
            batch_resp = rbind(batch_resp, curr_resp)
            rm(curr_resp)
            gc()
        }
        batch_resp = batch_resp[-1,]

        # write out the response per year
        cache_file = paste0(proj_dir, 
                                "/data/scraped_data/downloads/links_crawled_", 
                                curr_year, "_",
                                "guardian.json")
        write(jsonlite::toJSON(batch_resp), cache_file)

    }

}

#' public method that updates/fills the country+year cache. 
#' This method must be run by users the first time, 
#' since I am unable to share my mined data
#' 
#' @param api_key, API Key for Springer API, the user must make their own
#' this can be attained through registration here: 
#' https://dev.springernature.com/signup?cannot_be_converted_to_param
initialize_guardian_query <- function(api_key){

    # query each month for each year
    year_vec = 2005:2020
    month_vec = 1:12

    batch_resp = batch_guardian_query(year_vec, month_vec, api_key)

    return(batch_resp)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
resp = initialize_guardian_query(api_key)
