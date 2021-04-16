require(tidyr)
require(jsonlite)
require(data.table)
require(dplyr)

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
#' Populates reference_data/springer_cited_author_country_cache.tsv as a side-effect
#' 
#' @param curr_doi, a DOI
#' @param api_key, personal API_KEY
#' @return dataframe of author affiliation country info for each DOI
springer_country_doi_query <- function(curr_file, curr_doi, api_key){

    # we need to follow Springer guidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second


    #run query
    print(paste("running query"))
    query_url = url_springer_doi_search(curr_doi, api_key)
    resp = internal_springer_query(query_url)

    # format response
    resp_df = NA
    if(!all(is.na(resp)) & resp$result$total > 0){
        if(dim(resp$facets$values[5][[1]])[1] > 0){
            resp_df = as.data.frame(resp$facets$values[5])
            colnames(resp_df) = c("country", "num_entries")
            resp_df$doi = curr_doi
            resp_df$file_id = curr_file
        }
        

    }

    # get metadata
    if(!is.na(resp_df)){
        query_date = as.Date(Sys.Date(), format = "%B %d %Y")
        resp_df$query_date = query_date    
    }


    Sys.sleep(0.2)

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
file_id_batch_springer_doi_query <- function(dois_found_df, api_key, outdir){

    # query within each article id, then batched at a max size of 50
    # at a time and append together
    batch_resp = NA

    idx = 1
    for(curr_doi in unique(dois_found_df$doi)){
        print(curr_doi)
        curr_file = dois_found_df$file_id[idx]

        curr_resp = springer_country_doi_query(curr_file, curr_doi, api_key)
        if(!is.na(curr_resp)){
            batch_resp = rbind(batch_resp, curr_resp)
        }


        if(is.na(curr_resp)){
            # we don't want to query twice so even if its empty, we enter it as null
            curr_summ_df = data.frame(doi=curr_doi,
                                    file_id=curr_file,
                                    country=NA,
                                    query_date=as.Date(Sys.Date(), format = "%B %d %Y"),
                                    num_entries=NA)
        }else{
            # summarize further in case multiple queries per file-id
            curr_summ_df = curr_resp
            curr_summ_df$num_entries = as.numeric(curr_summ_df$num_entries)
            curr_summ_df = curr_summ_df %>% 
                        group_by(doi, file_id, country, query_date) %>% 
                        summarise(sum_entries=sum(num_entries)) 
            curr_summ_df = data.frame(curr_summ_df)
            colnames(curr_summ_df)[5] = "num_entries"

        }
        outfile = file.path(outdir, "cited_author_country.tsv")
        cached_df = data.frame(fread(outfile))
        cached_df = rbind(cached_df, curr_summ_df)
        write.table(cached_df, file=outfile, sep="\t", quote=F, row.names=F)


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
get_ref_authors <- function(api_key, ref_dir, outdir){

    # get the doi info
    ref_dois_df = get_ref_dois(ref_dir)

    # already cached springer dataframe
    # we know which are in springer, so lets not do it again
    gender_cache_file = file.path(ref_data_dir, "/springer_cited_author_cache.tsv")
    gender_cache_df = data.frame(fread(gender_cache_file))
    gender_cache_df = unique(na.omit(gender_cache_df[,c("doi", "publisher")]))
    dois_found_df = merge(ref_dois_df, gender_cache_df, by="doi")

    # only query for files we havent already seen
    outfile = file.path(outdir, "cited_author_country.tsv")
    cached_df = data.frame(fread(outfile))
    dois_found_df = subset(dois_found_df, !doi %in% unique(cached_df$doi))

    print(length(unique(dois_found_df$doi)))

    batch_resp = file_id_batch_springer_doi_query(dois_found_df, api_key, outdir)

}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
api_key = args[1]
ref_dir = args[2]
outdir = args[3]
get_ref_authors(api_key, ref_dir, outdir)
