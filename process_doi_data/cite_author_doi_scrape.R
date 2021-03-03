require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scrape_utils.R"))



#' public method that makes a single query to Springer.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' Populates reference_data/springer_cited_author_cache.tsv as a side-effect
#' 
#' @param doi_chunk, a vector of DOIs
#' @param api_key, personal API_KEY
#' @return dataframe of author info for each DOI
springer_doi_query <- function(doi_chunk, api_key){

    # we need to follow Springer guidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    query_df = NA
    cache_file = file.path(ref_data_dir, "/springer_cited_author_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_cited_author_cache file not found, will be created")
        cache_df = NA
    }else{
        # only hit the API for doi's that haven't been found yet
        cache_df = data.frame(fread(cache_file))
        query_df = subset(cache_df, doi %in% doi_chunk)
        
        #doi_chunk has the remaining
        doi_chunk = setdiff(doi_chunk, query_df$doi)
        if(length(doi_chunk) == 0){
            return(query_df)
        }
    }


    #run query
    print(paste("running query"))
    query_url = url_springer_doi_search(doi_chunk, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = NA
    if(!all(is.na(resp)) & resp$result$total > 0){
        na_elem = rep(NA, as.numeric(resp$result$total))
        resp_df = data.frame("doi" = na_elem,
                        "content_type" = na_elem,
                        "article_type" = na_elem,
                        "authors" = na_elem,
                        "publisher" = na_elem,
                        "year" = na_elem)

        resp_df$doi = paste("doi:", resp$records$doi, sep="")
        resp_df$content_type = resp$records$contentType
        resp_df$article_type = unlist(lapply(resp$records$genre,
                                    function(x) paste(x, collapse="; ")))
        resp_df$authors = unlist(lapply(resp$records$creators,
                                function(x) 
                                    paste(x$creator, collapse="; ")))
        resp_df$publisher = resp$records$publisher
        resp_df$year = substr(resp$records$publicationDate, 1, 4)
    }



    # add in missed DOIs
    all_dois = doi_chunk
    if(!all(is.na(query_df))){
        all_dois = union(doi_chunk, query_df$doi)
    }
    if(!all(is.na(resp_df))){
        missed_dois = setdiff(all_dois, resp_df$doi)
    }else{
        missed_dois = all_dois
    }
    if(length(missed_dois) != 0){
        na_elem = rep(NA, length(missed_dois))
        missed_df = data.frame("doi" = missed_dois,
                        "content_type" = na_elem,
                        "article_type" = na_elem,
                        "authors" = na_elem,
                        "publisher" = na_elem,
                        "year" = na_elem)

        if(!all(is.na(resp_df))){
            resp_df = rbind(resp_df, missed_df)
        }else{
            resp_df = missed_df
        }
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
    cache_file = file.path(ref_data_dir, "/springer_cited_author_cache.tsv")
    write.table(cache_df, cache_file, sep="\t", quote=F, row.names=F)

    Sys.sleep(1)

    # return the new resp and the cached resp
    resp_df = rbind(query_df, resp_df)

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
batch_springer_doi_query <- function(doi_vec, api_key){

    # query one at a time and append together
    batch_resp = NA
    doi_chunks = split(doi_vec, ceiling(seq_along(doi_vec)/50))
    idx = 1
    for(chunk in doi_chunks[1:length(doi_chunks)]){
        curr_resp = springer_doi_query(chunk, api_key)
        if(!is.na(curr_resp)){
            batch_resp = rbind(batch_resp, curr_resp)
        }
        print(idx)
        idx = idx +1 
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
get_ref_authors <- function(api_key, ref_dir){

    # get the authors from each article
    ref_dois_df = get_ref_dois(ref_dir)

    # query each doi for authors
    doi_vec = unique(ref_dois_df$doi)
    batch_resp = batch_springer_doi_query(doi_vec, api_key)

    return(batch_resp)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
ref_dir = args[2]
get_ref_authors(api_key, ref_dir)
