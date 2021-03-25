require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))



#' public method that makes a single query to Springer.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' Populates reference_data/springer_random_country_cache.tsv as a side-effect
#' 
#' @param doi_chunk, a vector of DOIs
#' @param api_key, personal API_KEY
#' @return dataframe of author info for each DOI
springer_doi_rand_query <- function(doi_chunk, curr_year, api_key){

    # we need to follow Springer guidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    cache_file = file.path(ref_data_dir, "/springer_random_country_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_random_country_cache file not found, will be created")
        cache_df = NA
    }else{
        cache_df = data.frame(fread(cache_file))
        query_df = subset(cache_df, doi == doi_chunk)
        if(nrow(query_df) != 0){
            return(query_df)
        }
    }


    #run query
    print(paste("running query"))
    query_url = url_springer_doi_search(doi_chunk, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = data.frame(country=NA,
                        num_entries=NA,
                        total=1,
                        doi=doi_chunk,
                        year = curr_year)
    if(!all(is.na(resp)) & resp$result$total > 0 & length(resp$facets[5,2][[1]])){

        resp_df = resp$facets[5,2][[1]]
        colnames(resp_df) = c("country", "num_entries")

        resp_df$total = resp$result$total
        resp_df$doi = doi_chunk
        resp_df$year = curr_year

    }


    # get metadata
    query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    resp_df$query_date = query_date    

    
    # append to the cache
    cache_file = file.path(ref_data_dir, "/springer_random_country_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_random_country_cache file not found, will be created")
        cache_df = NA
    }else{
        cache_df = data.frame(fread(cache_file))
    }
    if(!all(is.na(cache_df))){
        # format the cache data frame
        resp_df = resp_df[,colnames(cache_df)]
        cache_df = rbind(cache_df, resp_df)
    }else{
        cache_df = resp_df
    }
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
#' @param doi_vec, all doi's to query
#' @param api_key, personal API_KEY
#' @return dataframe of author info from Springer
batch_springer_doi_rand_query <- function(author_dois_df, api_key){

    # query one at a time and append together
    batch_resp = NA
    for(curr_year in 2011:2020){ #unique(author_dois_df$year)
        curr_df = subset(author_dois_df, year == curr_year)
        doi_vec = unique(curr_df$doi)
        # we can only do 5000 queries, so we will randomly choose 250 to query
        set.seed(5)
        doi_vec = sample(doi_vec, 250)
        idx = 1
        for(chunk in doi_vec[1:250]){
            curr_resp = springer_doi_rand_query(chunk, curr_year, api_key)
            if(!is.na(curr_resp)){
                batch_resp = rbind(batch_resp, curr_resp)
            }
            print(idx)
            idx = idx +1 
        }

    }
    batch_resp = batch_resp[-1,]
    return(batch_resp)

}


#' public method that updates/fills the springer author doi cache. 
#' This method must be run by users the first time, 
#' since I am unable to share my mined data
#' 
#' @param api_key, API Key for Springer API, the user must make their own
#' this can be attained through registration here: 
#' https://dev.springernature.com/signup?cannot_be_converted_to_param
get_ref_authors <- function(api_key, author_file){

    # get the authors from each article
    author_dois_df = data.frame(fread(author_file))


    # query each doi for authors
    batch_resp = batch_springer_doi_rand_query(author_dois_df, api_key)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
author_file = args[2]
get_ref_authors(api_key, nature_dir)
