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


#' Read all nature json files and get author info
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
#' @return dataframe, all_authors author infos for nature articles
read_nature_author_json_files <- function(nature_dir){

    json_res_files = list.files(nature_dir, pattern=".json", full.names = TRUE)
    
    all_authors = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        # format authors
        authors = unlist(lapply(json_res$authors, function(x) paste(unlist(x$name), collapse="; ")))

        # make df
        authors_df = data.frame(file_id=json_res$file_id,
                                year=json_res$year,
                                authors=authors)
        
        all_authors = rbind(all_authors, authors_df)

    }

    all_authors = all_authors[-1,]

    # format file_id into a doi
    all_authors$doi = paste("doi:10.1038/", all_authors$file_id, sep="")

    return(all_authors)

}


#' Read all nature json files and get author info
#' @param author_df, data.frame with all author info 
#' 
#' @return dataframe, reducing all author info to first and last authors only
format_authors <- function(author_df, use_fullname=F){

    library(stringr)

    # only keep publications with more than 1 author
    author_df = author_df[grep(";", author_df$authors),]

    first_authors = unlist(lapply(author_df$authors, function(x) unlist(str_split(x, "; "))[1]))
    last_authors = unlist(lapply(author_df$authors, function(x) rev(unlist(str_split(x, "; ")))[1]))

    if(!use_fullname){
        first_authors = format_author_firstnames(first_authors)
        last_authors = format_author_firstnames(last_authors)
    }else{
        first_authors = format_author_fullname(first_authors)
        last_authors = format_author_fullname(last_authors)
    }

    first_author_df = data.frame(doi = author_df$doi,
                                year = author_df$year,
                                author_pos = "first",
                                author = first_authors)
    last_author_df = data.frame(doi = author_df$doi,
                                year = author_df$year,
                                author_pos = "last",
                                author = last_authors)

    if(length(grep("file_id", colnames(author_df))) != 0){
        first_author_df$file_id = author_df$file_id
        last_author_df$file_id = author_df$file_id
    }

    author_df = rbind(first_author_df, last_author_df)


    author_df = unique(author_df)
    return(author_df)

}

get_author_gender <- function(unknown_gendered_df){

    unknown_gendered_df$author = tolower(unknown_gendered_df$author)
    names_missing = data.frame(author=unique(unknown_gendered_df$author))

    gender_io_file = file.path(proj_dir, "/data/reference_data/genderize.tsv")
    names_processed = data.frame(fread(gender_io_file))
    gender_io_file = file.path(proj_dir, "/data/reference_data/genderize_update.tsv")
    names_processed = rbind(names_processed, data.frame(fread(gender_io_file)))

    colnames(names_processed)[1] = "author"
    names_processed$guessed_gender = NA
    names_processed$guessed_gender[names_processed$probability_male < 0.5] = "FEMALE"
    names_processed$guessed_gender[names_processed$probability_male >= 0.5] = "MALE"

    # guess genders from reference
    names_processed = merge(data.table(names_missing), 
                            data.table(names_processed),
                            all.x=T)

    names_processed = data.frame(unique(names_processed))

    # save these to add to the reference dataset later
    names_not_processed = names_processed$author[is.na(names_processed$query_date)]

    unknown_gendered_df = merge(unknown_gendered_df, 
                                unique(names_processed[,c("author", "guessed_gender")]),
                                all.x=T)
    unknown_gendered_df$gender = unknown_gendered_df$guessed_gender

    return(list(names_not_processed, unknown_gendered_df))

}

