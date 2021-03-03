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
#' Populates reference_data/springer_cited_author_country_cache.tsv as a side-effect
#' 
#' @param doi_chunk, a vector of DOIs
#' @param api_key, personal API_KEY
#' @return dataframe of author affiliation country info for each DOI
springer_country_doi_query <- function(year, doi_chunk, api_key){

    # we need to follow Springer guidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second


    #run query
    print(paste("running query"))
    query_url = url_springer_doi_search(doi_chunk, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = NA
    if(!all(is.na(resp)) & resp$result$total > 0){
        
        resp_df = as.data.frame(resp$facets$values[5])
        colnames(resp_df) = c("country", "num_entries")
        resp_df$year = year

    }

    # get metadata
    query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    resp_df$query_date = query_date    


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
year_batch_springer_doi_query <- function(dois_found_df, api_key){

    # query within each year, then batched at a max size of 50
    # at a time and append together
    batch_resp = NA
    for(curr_year in unique(dois_found_df$year)){
        doi_vec = dois_found_df$doi[which(dois_found_df$year == curr_year)]
        doi_chunks = split(doi_vec, ceiling(seq_along(doi_vec)/50))
        idx = 1
        print(curr_year)
        for(chunk in doi_chunks[1:length(doi_chunks)]){
            curr_resp = springer_country_doi_query(curr_year, chunk, api_key)
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
get_ref_authors <- function(api_key, ref_dir, outdir){

    # get the doi info
    ref_dois_df = get_ref_dois(ref_dir)

    # already cached springer dataframe
    cache_file = file.path(ref_data_dir, "/springer_cited_author_cache.tsv")
    cache_df = data.frame(fread(cache_file))
    cache_df = unique(na.omit(cache_df[,c("doi", "publisher")]))
    dois_found_df = merge(ref_dois_df, cache_df)

    batch_resp = batch_springer_doi_query(dois_found_df, api_key)

    # summarize further in case multiple queries per year
    batch_summ_df = batch_resp
    batch_summ_df$num_entries = as.numeric(batch_summ_df$num_entries)
    batch_summ_df = batch_summ_df %>% 
                group_by(year, country) %>% 
                summarise(sum_entries=sum(num_entries)) 
    batch_summ_df = data.frame(batch_summ_df)
    colnames(batch_summ_df)[3] = "num_entries"

    outfile = file.path(outdir, "cited_author_country.tsv")
    write.table(batch_summ_df, file=outfile, sep="\t", quote=F, row.names=F)

    return(batch_summ_df)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
ref_dir = args[2]
outdir = args[3]
get_ref_authors(api_key, ref_dir, outdir)
