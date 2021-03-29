require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))



#' private internal method that queries springer
#' 
#' @param string; URL formatted for springer API
#' @return OSM JSON response
internal_springer_query <- function(url_search){

    # block augmented from here: 
    # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    out <- tryCatch(
        {
            fromJSON(getURL(url_search))
        },
        error=function(cond) {
            message(paste("query error:", url_search))
            # Choose a return value in case of error
            return(NA)
        }
    )    
    return(out)
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
    if(length(grep("http://nature.com/doifinder/", doi))!= 0){
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

#' format query into Springer-appropriate URL query
#' 
#' @param doi_chunk, vector of dois
#' @param api_key, personal API_KEY
#' @return string, url for Springer API call
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
