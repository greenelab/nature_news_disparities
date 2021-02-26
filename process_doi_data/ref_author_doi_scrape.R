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
url_springer_doi_search <- function(doi_chunk, api_key) {

    # load libraries
    library(RCurl)

    if(length(doi_chunk) > 1){
        doi_str = paste(doi_chunk, "OR", collapse=" ")
        if(substr(doi_str, nchar(doi_str)-2, nchar(doi_str)) == " OR"){
            doi_str = substr(doi_str, 1, nchar(doi_str)-3)
        }
    }else if(length(doi_chunk) == 1){
        doi_str = doi_chunk
    }else{
        return(NA)
    }

    # search api url
    url_springer_search_api <- "http://api.springernature.com/metadata/json?q=( "
    query_str <- paste(url_springer_search_api, 
                        doi_str, ")",
                        "&api_key=", api_key,
                        "&p=", max(length(doi_chunk), 10),
                        sep="")


    # percent-encode search request
    single_query <- URLencode(query_str, repeated=T)
    
    return(single_query)
}



#' public method that makes a single query to Springer.
#' This method takes into account all Springer query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query Springer and 
#' update the cache
#' 
#' @param doi_vec, a vector of DOIs
#' @param api_key, personal API_KEY
#' @return dataframe result from OSM with formatted location data
springer_doi_query <- function(doi_chunk, api_key){

    # we need to follow Springer guidelines
    # we MUST cache
    # 5000 per day
    # no parallel processes
    # no more than 9 queries per second

    # make sure query is not in cache
    query_df = NA
    cache_file = file.path(ref_data_dir, "/springer_doi_cache.tsv")
    if(!file.exists(cache_file)){
        warning("springer_doi_cache file not found, will be created")
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
                        "publisher" = na_elem)

        resp_df$doi = paste("doi:", resp$records$doi, sep="")
        resp_df$content_type = resp$records$contentType
        resp_df$article_type = unlist(lapply(resp$records$genre,
                                    function(x) paste(x, collapse="; ")))
        resp_df$authors = unlist(lapply(resp$records$creators,
                                function(x) 
                                    paste(x$creator, collapse="; ")))
        resp_df$publisher = resp$records$publisher
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
                        "publisher" = na_elem)

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
    cache_file = file.path(ref_data_dir, "/springer_doi_cache.tsv")
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
#' @param year_vec, vector of years to query over
#' @param country_vec, vector of countries to query over
#' @param api_key, personal API_KEY
#' @return dataframe result from Springer
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


#' public method that formats doi's 
#' from the scraped data
#' 
#' @param doi, string doi from scraped article
#' @return formatted_doi, string
process_dois <- function(doi){

    require("stringr")
    options(useFancyQuotes = FALSE)


    # remove PNAS, plos and science journals
    if(length(grep("http://www.pnas.org_cgi_doi_", doi))!= 0){
        #doi = str_replace(doi, "http://www.pnas.org_cgi_doi_", "doi:")
        doi = ""
    }
    if(length(grep("http://www.pnas.org/cgi/doi/", doi))!= 0){
        #doi = str_replace(doi, "http://www.pnas.org/cgi/doi/", "doi:")
        doi = ""
    }
    if(length(grep("science", doi))!= 0){
        doi = ""
    }
    if(length(grep("science", doi))!= 0){
        doi = ""
    }



    # format the string

    # remove quotes
    if(length(grep("\"", doi, fixed=T))!= 0){
        doi = str_replace(doi, "\"", "")
    }

    # because of Api call issues, ignore doi's with < and >
    if(length(grep("&lt;|&gt;|<|>", doi, fixed=F))!= 0){
        doi = ""
    }

    # format scraped code
    if(length(grep("http://www.nature.com/doifinder/", doi))!= 0){
        doi = str_replace(doi, "http://www.nature.com/doifinder/", "doi:")
    }
    if(length(grep("http://nature.comdoi", doi))!= 0){
        doi = str_replace(doi, "http://nature.comdoi", "doi:")
    }
    if(length(grep("/doifinder/", doi))!= 0){
        doi = str_replace(doi, "/doifinder/", "doi:")
    }

    
    # if the last character is a . replace it
    if(substr(doi, nchar(doi), nchar(doi)) == "."){
        doi = substr(doi, 1, nchar(doi)-1)
    }

    # if the last character is an unmatched ) replace it
    if(substr(doi, nchar(doi), nchar(doi)) == ")"){
        str_counter = 0
        doi_split <- strsplit(doi, "")[[1]]
        for(idx in doi_split){
            if(idx == "("){
                str_counter = str_counter + 1
            }else if(idx == ")"){
                str_counter = str_counter - 1
            }
        }
        if(str_counter == -1){
            doi = substr(doi, 1, nchar(doi)-1)
        }
    }

    # if the scraped doi is messed up, we ignore
    if(length(grep("http", doi))!= 0){
        doi = ""
    }

    # if there is a () then this doi has to be quoted
    if(length(grep("(", doi, fixed=T))!= 0){
        doi_base = substr(doi, 5, nchar(doi))
        doi = paste("doi:", dQuote(doi_base), sep="")
    }


    return(doi)


}

#' public method that gets all the DOIs from the 
#' references in scraped articles. 
#' 
#' @param ref_dir, directory of JSON files with DOIs from references
#' @return dataframe of all dois with metadata
get_ref_dois <- function(ref_dir){
        
    json_res_files = list.files(ref_dir, pattern=".json", full.names = TRUE)
    
    all_ref_dois = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        # get file meta data
        file_id = basename(curr_file)
        file_year = substr(file_id, 15, 18)
        file_type = substr(file_id, 20, nchar(file_id)-5)
        
        # skip empty files
        if(file.info(curr_file)$size == 0){
            next
        }

        # read and format JSON
        json_df = data.frame(fromJSON(curr_file))
        json_df = data.frame(separate_rows(json_df, dois, sep=", "))

        doi_df = json_df
        doi_df$year = file_year
        doi_df$type = file_type

        # only get the files with actual dois
        doi_df = subset(doi_df, dois != "")

        if(nrow(doi_df) != 0){
            all_ref_dois = rbind(all_ref_dois, doi_df)
        }

    }
    all_ref_dois = all_ref_dois[-1,]

    # format 
    colnames(all_ref_dois)[which(colnames(all_ref_dois) == "dois")] = "doi"
    all_ref_dois$doi = unlist(lapply(all_ref_dois$doi, process_dois))
    all_ref_dois = subset(all_ref_dois, doi != "")


    return(all_ref_dois)
}

#' public method that updates/fills the country+year cache. 
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
