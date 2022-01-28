require(data.table)
require(tidyr)
require(here)
require(jsonlite)
require(humaniformat)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")


pronouns = c("i", "me", "my", "mine", "myself",
            "you", "your", "yours", "yourself",
            "he", "him", "his", "himself",
            "she", "her", "hers", "herself",
            "it", "its", "itself",
            "we", "us", "our", "ours", "ourselves",
            "yourselves",
            "they", "them", "their", "theirs", "themselves")
gender = c(NA, NA, NA, NA, NA,
            NA, NA, NA, NA,
            "MALE", "MALE", "MALE", "MALE",
            "FEMALE", "FEMALE", "FEMALE", "FEMALE",
            NA, NA, NA,
            NA, NA, NA, NA, NA,
            NA,
            NA, NA, NA, NA, NA)

pronouns_df = data.frame(pronouns, gender)

#' read in JSON file and convert to dataframe
#' 
#' @param infile, JSON file path
#' 
#' @return json_res, dataframe version of JSON object
read_json <- function(infile){

    json_res <- tryCatch(
        {
           fromJSON(infile)
        },
        error=function(cond) {
            message(paste("JSON file empty or error:", infile))
            message("The original error message:")
            message(cond)
            return(NA)
        }
    )
    json_res = data.frame(json_res)
    return(json_res)

}

#' Normalize name order and initials
#' using the humaniformat package
#' 
#' @param in_name, Name in any format
#' 
#' @return normalized first name
format_author_firstnames_internal <- function(in_name){
    # reverse name if needed
    in_name = format_reverse(in_name)
    in_name = format_period(in_name)
    return(first_name(in_name))

}

#' Normalize and filter first names
#' that are not initials or companies/groups
#' 
#' @param author_vec, vector of names in any format
#' 
#' @return normalized first names
format_author_firstnames <- function(author_vec){

    author_vec = unlist(lapply(author_vec, format_author_firstnames_internal))

    # now remove anything that looks like a consortium or not a name
    non_name_check = "consortium|group|initiative|team|collab|committee|center|program|author|institute"
    non_name_idx = grep(non_name_check, author_vec)
    author_vec[non_name_idx] = ""


    # now remove anything that looks like initials
    # so this means it has at least 1 period or - and < 4 characters
    non_name_idx = grep("[.-]", author_vec)
    short_name_idx = which(unlist(lapply(author_vec, nchar)) < 4)
    author_vec[intersect(non_name_idx, short_name_idx)] = ""

    # now strip periods
    author_vec = gsub("[.]", "", author_vec)

    # or less than 3 characters
    short_name_idx = which(unlist(lapply(author_vec, nchar)) < 2)
    author_vec[short_name_idx] = ""


    # make sure we are only getting the first name
    author_vec = unlist(lapply(author_vec, function(x) unlist(str_split(x, " "))[1]))
    

    return(author_vec)
}

#' Normalize name order and initials
#' using the humaniformat package
#' 
#' @param in_name, Name in any format
#' 
#' @return normalized first name
format_author_fullname_internal <- function(in_name){
    # reverse name if needed
    in_name = format_reverse(in_name)

    # format any initials
    in_name = format_period(in_name)

    # extract name
    in_name = parse_names(in_name)

    # format first name
    in_name$first_name = format_author_firstnames(in_name$first_name)
    if(in_name$first_name == "" | is.na(in_name$first_name)){
        return("")
    }

    in_name = in_name[c("first_name", "last_name")]
    in_name = paste(na.omit(unlist(in_name[1,])), collapse=" ")
    in_name = str_trim(in_name)

    # if the name is less than 3 characters remove
    if(nchar(in_name) <= 3){
        in_name = ""
    }

    return(in_name)

}

#' Normalize and filter first names
#' that are not initials or companies/groups
#' 
#' @param author_vec, vector of names in any format
#' 
#' @return normalized first names
format_author_fullname <- function(author_vec){

    author_vec = unlist(lapply(author_vec, format_author_fullname_internal))

    # now remove anything that looks like a consortium or not a name
    non_name_check = "consortium|group|initiative|team|collab|committee|center|program|author|institute"
    non_name_idx = grep(non_name_check, author_vec)
    author_vec[non_name_idx] = ""


    return(author_vec)
}


#' Format string version of a name
#'
#' @param in_str, string; a name
#' @return format_str, string; formatted name 
#' (no white space, no non-letter characters)
format_name_str <- function(in_str){
    require("stringr")

    # remove 's
    format_str = str_replace(in_str, "'s", "")


    # remove any weird characters .
    format_str = gsub(
        "[-.()!@#$%^&*_+=]",
        "", 
        format_str, 
        fixed=F)

    format_str = gsub("[", "", format_str, fixed=T)
    format_str = gsub("]", "", format_str, fixed=T)

    # remove single characters
    # taken from 
    #https://datascience.stackexchange.com/questions/16976/r-remove-single-characters-from-string
    format_str = paste(Filter(function(x) nchar(x) > 1,
                                unlist(strsplit(format_str, " "))), 
                        collapse=" ")


    # remove trailing white space
    format_str = trimws(format_str)

    return(format_str)

}

#' Given a query string, return closest string in the
#' vector of reference strings (no penalty for deletion)
#' using adist: "the generalized Levenshtein (edit) distance, 
#' giving the minimal possibly weighted number of insertions, 
#' deletions and substitutions needed to transform one string into another."
#' For our use, deletions are NOT penalized and have weight 0
#' this is because when matching names, we may only get a last name 
#' and would like to match it to a full name. 
#' Example query string: "John"
#' reference strings: "John Fillipio", "Jane Do"
#' without deletion weight of 0, John has the closest distance to "Jane Doe"
#' 
#' 
#' @param key_str, a query string 
#' @param str_vec, a string vector
#' @return string, a string from str_vec that has closest distance to key_str
get_matched_string <- function(key_str, str_vec, max_cost=NA){
    require("stringdist")

    # get closest matching string
    costs = sapply(tolower(str_vec), adist, y=tolower(key_str), costs=list(deletions=0))
    lcs_idx = which(costs == min(costs)[1])

    if(!is.na(max_cost)){
        if(min(costs)[1] > max_cost){
            return(NA)
        }
    }
    return(str_vec[lcs_idx])

}

#' get all named entities found in coreNLP output JSON
#'
#' @param json_res, JSON object that was output from coreNLP
#' @return dataframe of all named entities in json_res
get_ner <- function(json_res){

    # get named entities
    ner_list = json_res$sentences$entitymentions
    ner_list = ner_list[lapply(ner_list,length)>0]
    ner_list = lapply(ner_list, function(x) x[,c("text", "ner")])
    ner_df = do.call(rbind, ner_list)

    return(ner_df)
}

#' read in and return the reference country information
#'
#' @return dataframe of all reference country information
get_country_info <- function(){

    # now get the UN region info
    country_file = file.path(ref_data_dir, "cdh_country_codes.txt")
    if(!file.exists(country_file)){
        stop("cdh_country_codes file not found, please run setup.sh to get it")
    }

    # format country file
    country_df = data.frame(fread(country_file, skip=1))
    colnames(country_df) = c("idx_name", "country_alt_name", "ISO.3166.1.COUNTRY.CHAR.2.CODE",
                                "ISO.3166.1.COUNTRY.CHAR.3.CODE", "ISO.3166.1.COUNTRY.NUMBER.CODE", 
                                "FIPS.COUNTRY.CODE", "FIPS.COUNTRY.NAME", "UN.REGION",
                                "UN.SUB.REGION.NAME", "CDH.ID")
    colnames(country_df)[which(colnames(country_df) == "FIPS.COUNTRY.NAME")] = "country"
    colnames(country_df)[which(colnames(country_df) == "ISO.3166.1.COUNTRY.CHAR.2.CODE")] = "address.country_code"
    colnames(country_df)[which(colnames(country_df) == "UN.REGION")] = "un_region"
    colnames(country_df)[which(colnames(country_df) == "UN.SUB.REGION.NAME")] = "un_subregion"
    country_df = country_df[,c( "country", "address.country_code", "un_region", "un_subregion")]
    country_df$address.country_code = tolower(country_df$address.country_code)

    return(country_df)
}

#' get the OSM location information for all 
#' organization, country, or state/province NERs 
#' identified in corenLP JSON output.
#' OSM location information first comes from the cache, 
#' if not found it will query the online database
#'
#' @param json_res, JSON object that was output from coreNLP
#' @return dataframe of all OSM information for each location NER
get_osm_locations <- function(json_res){

    # get named entities --locations
    ner_df = get_ner(json_res)

    if(length(ner_df) == 0){
        return(NA)
    }

    ner_locs_df = subset(ner_df, ner %in% 
                        c("ORGANIZATION", "COUNTRY", "STATE_OR_PROVINCE"))
    ner_locs_df = unique(ner_locs_df)


    if(length(ner_locs_df) == 0){
        return(NA)
    }

    if(all(is.na(ner_locs_df))){
        return(NA)
    }

    # now query open street map to get the country codes
    ner_locs_df$text = tolower(ner_locs_df$text)
    osm_res = batch_osm_query(unique(ner_locs_df$text))
    colnames(osm_res)[which(colnames(osm_res) == "query")] = "text"
    ner_locs_df = merge(ner_locs_df, 
                        osm_res[,c("text", "address.country_code")],
                        all=T)
    ner_locs_df$address.country_code[
        which(is.na(ner_locs_df$address.country_code))] = "NOT_FOUND"

    locs_df = unique(ner_locs_df)

    return(locs_df)

}


#' get all individual names from coreNLP output
#' The names either come from NERs.
#' Partial names are also matched to full names
#' using the function get_matched_string
#' 
#' @param json_res, JSON object that was output from coreNLP
#' @return dataframe of all the name information and a first guess at gender
get_persons <- function(json_res){

    # get named entities --names
    ner_df = get_ner(json_res)
    if(length(ner_df) == 0){
        return(NA)
    }

    ner_names_df = subset(ner_df, ner=="PERSON")
    ner_names_df = subset(ner_names_df, !tolower(text) %in% pronouns)

   # if there are no names, return NA
    if(nrow(ner_names_df) == 0){
        print("no speakers found")
        return(NA)
    }
    ner_names_df = data.frame(text=ner_names_df$text, gender="UNKNOWN")

 
    # now find the longest strings and gender
    max_str = c()
    str_to_iter = unique(ner_names_df$text)
    str_to_iter = str_to_iter[order(nchar(str_to_iter), str_to_iter, decreasing=T)]
    for(curr_text in str_to_iter){

        curr_text = format_name_str(curr_text)

        str_idx = grep(curr_text, max_str)
        if(length(str_idx) == 0){
            # string not found, add it
            max_str = c(max_str, curr_text)
        }else if(length(str_idx) > 1){
            # the substring found twice
            # so this means if you are searching for John
            # you could get back John Houghton or John Lee
            # lets just ignore it
            print("ambiguous speakers, (names are too similar)")
            print(curr_text)
        }else{
            old_str = max_str[str_idx]
            if(nchar(old_str) < nchar(curr_text)){
                max_str[str_idx] = curr_text
            }
        }
    }

    # now convert names
    names_df = ner_names_df
    names_df$full_name = NA
    for(idx in 1:nrow(names_df)){
        curr_text = names_df$text[idx]
        curr_text = format_name_str(curr_text)

        names_df$full_name[idx] = get_matched_string(curr_text, max_str)
    }

    # if the gender is known, keep it, and remove duplicate names
    # relies on FEMALE + MALE is sorted before UNKNOWN
    gender_df = names_df[,c("full_name", "gender")]
    gender_df = gender_df[order(gender_df$gender),]
    gender_df = gender_df[which(!duplicated(gender_df$full_name)), ]

    final_df = unique(names_df[,c("full_name", "text")])
    final_df = merge(final_df, gender_df, all.x=T)
    colnames(final_df) = c("full_name", "partial_name", "gender")

    return(final_df)

}


#' format query into OSM-appropriate URL query
#' 
#' @param single_query, string to query OSM
#' @return string, url
url_nominatim_search <- function(single_query) {
    # this code is augmented from 
    # https://towardsdatascience.com/breaking-down-geocoding-in-r-a-complete-guide-1d0f8acd0d4b
    # load libraries
    require(RCurl)
    # nominatim search api url
    url_nominatim_search_api <- "https://nominatim.openstreetmap.org/search/"


    # percent-encode search request
    single_query <- URLencode(single_query)
    parameters_url <- paste0("?format=json",
                             "&addressdetails=1&limit=1")
    # construct search request for geocode
    url_nominatim_search_call <- paste0(url_nominatim_search_api,
                                        single_query, parameters_url)


    return(url_nominatim_search_call)
}


#' private internal method that queries OSM
#' 
#' @param curr_q, string to query OSM
#' @return OSM JSON response
internal_osm_query <- function(curr_q){

    url_search = url_nominatim_search(curr_q)

    # block augmented from here: 
    # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    out <- tryCatch(
        {
            getURL(url_search, httpheader=c('User-Agent'="nature news scraper v0.1"))
        },
        error=function(cond) {
            message(paste("query error:", curr_q))
            # Choose a return value in case of error
            return(NA)
        }
    )    
    return(out)
}


#' public method that makes a single query to OSM.
#' This method takes into account all OSM query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query OSM and 
#' update the cache
#' 
#' @param curr_q, string to query OSM
#' @return dataframe result from OSM with formatted location data
single_osm_query <- function(curr_q){

    # we need to follow OSM quidelines
    # we MUST cache
    # no parallel processes
    # no more than one query per second

    require(tmaptools)

    # make sure query is not in cache
    cache_file = file.path(ref_data_dir, "/osm_cache.tsv")
    if(!file.exists(cache_file)){
        stop("osm_cache file not found, file should be in git, 
                you must download this before querying locations")
    }

    cache_df = data.frame(fread(cache_file))
    if(tolower(curr_q) %in% tolower(cache_df$query)){
        return(subset(cache_df, tolower(query) == tolower(curr_q)))
    }

    # strip out any URL specific characters 
    curr_q = tolower(curr_q)
    curr_q = gsub("[[:punct:]]", "", curr_q)
    
    #run query
    print(paste("running query:", tolower(curr_q)))
    resp = internal_osm_query(tolower(curr_q))
    resp_df = data.frame("place_id" = NA,
                            "osm_type" = NA, "display_name" = NA,
                            "class" = NA, "type" = NA, "importance" = NA,
                            "address.country" = NA, "address.country_code" = "NONE",
                            "Freq" = NA, "hand_edited" = FALSE, "reviewed" = FALSE)

    
    if(resp != "[]"){
        resp = unlist(fromJSON(resp,simplifyVector = FALSE))

        # make sure its in a country
        in_country = all(c("address.country", "address.country_code") %in% names(resp))
        if(!in_country){
            resp_df = data.frame(t(resp[c("place_id", "osm_type", 
                                "display_name", "class", "type", 
                                "importance")]))
            resp_df$address.country = NA
            resp_df$address.country_code = "NONE"
        }
        else{
            resp_df = data.frame(t(resp[c("place_id", "osm_type", 
                                "display_name", "class", "type", 
                                "importance", "address.country", 
                                "address.country_code")]))
        }
    }
    
    # if no OSM type, make sure its null
    if(!"osm_type" %in% colnames(resp_df)){
        resp_df$osm_type = NA
    }

    #process query info
    resp_df$query = curr_q

    # get metadata
    query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    resp_df$query_date = query_date

    ref_code_df = get_country_info()
    resp_df = merge(resp_df, ref_code_df, all.x=T)
    resp_df$Freq = NA
    resp_df$hand_edited = FALSE
    resp_df$reviewed = FALSE
    

    # format the data frame
    resp_df = resp_df[,colnames(cache_df)]


    # append to the cache
    cache_df = rbind(cache_df, resp_df)
    cache_file = file.path(ref_data_dir, "/osm_cache.tsv")
    write.table(cache_df, cache_file, sep="\t", quote=F, row.names=F)

    Sys.sleep(1)

    return(resp_df)


}

#' public method that makes multiple queries to OSM.
#' This method takes into account all OSM query guidelines
#' It first checks the cache for the query,
#' if it is not there then it will query OSM and 
#' update the cache
#' 
#' @param query_vec, vector of strings to query OSM
#' @return dataframe result from OSM with formatted location data
batch_osm_query <- function(query_vec){

    # query one at a time and append together
    batch_resp = NA
    for(curr_q in query_vec){

        curr_resp = single_osm_query(curr_q)

        batch_resp = rbind(batch_resp, curr_resp)

    }
    batch_resp = batch_resp[-1,]
    return(batch_resp)

}

#' private method that runsupdates the frequency of the OSM
#' cache. 
#' This method should not be run by users since they 
#' should always have a populated cache.
#' 
#' @param in_dir, full location of folder that contains all location tables
#' from pipeline step 4. Files must contain the string location_table_raw
#' in the name
#' @param all_authors, output df from 
#' /process_doi_data/process_author_country.R
#' read_nature_country_json_files
update_osm_freq <- function(in_dir, all_authors){

    # this method will just populate the cache with any locations
    # that are missing

    # people should never use this method, this is only
    # for me to run the first instance of the cache, others
    # should already have a populated cache file from github

    # find all files that unfound locations
    all_loc_files = list.files(in_dir, 
                                pattern="location_table_raw",
                                recursive=F,
                                full.names=T)
    if(length(all_loc_files) == 0){
        warn_str = "No location files found. Are you supplying the correct input dir?"
        warning(warn_str)
        return()
    }

    # read in location files for mentions
    all_missing_locs = NA
    for(curr_file in all_loc_files){
        in_df = data.frame(fread(curr_file))
        all_missing_locs = rbind(all_missing_locs, in_df[,c("file_id", "text")])
    }
    all_missing_locs = all_missing_locs[-1,]
    query_locs = unique(tolower(all_missing_locs$text))

    # read in location files for nature BG affiliations
    all_authors$text = all_authors$country_affil
    query_locs = unique(tolower(c(query_locs, all_authors$text)))
    all_missing_locs = rbind(all_missing_locs, all_authors[,c("file_id", "text")])

    # write out the frequncies of organizations, 
    # we want to make sure that the top ones are correct
    freq_locs = as.data.frame(table(all_missing_locs$text))
    colnames(freq_locs)[1] = "query"
    colnames(freq_locs)[2] = "Freq_update_05_24_2021"

    cache_file = file.path(ref_data_dir, "/osm_cache.tsv")
    cache_df = data.frame(fread(cache_file))
    freq_locs = merge(freq_locs, cache_df)
    freq_locs = freq_locs[order(freq_locs$Freq_update_05_24_2021, decreasing=T),]

    cache_edit_file = file.path(ref_data_dir, "/osm_cache_edited2.tsv")
    write.table(freq_locs, cache_edit_file, sep="\t", quote=F, row.names=F)


}

#' private method that runs the first population of the OSM
#' cache. 
#' This method should not be run by users since they 
#' should always have a populated cache.
#' 
#' @param in_dir, full location of folder that contains all location tables
#' from pipeline step 4. Files must contain the string location_table_raw
#' in the name
#' @param all_authors, output df from 
#' /process_doi_data/process_author_country.R
#' read_nature_country_json_files
initial_osm_query <- function(in_dir){

    # this method will just populate the cache with any locations
    # that are missing

    # people should never use this method, this is only
    # for me to run the first instance of the cache, others
    # should already have a populated cache file from github

    # find all files that unfound locations
    all_loc_files = list.files(in_dir, 
                                pattern="location_table_raw",
                                recursive=F,
                                full.names=T)
    if(length(all_loc_files) == 0){
        warn_str = "No location files found. Are you supplying the correct input dir?"
        warning(warn_str)
        return()
    }

    # read in location files for mentions
    all_missing_locs = NA
    for(curr_file in all_loc_files){
        in_df = data.frame(fread(curr_file))
        all_missing_locs = rbind(all_missing_locs, in_df[,c("file_id", "text")])
    }
    all_missing_locs = all_missing_locs[-1,]
    query_locs = unique(tolower(all_missing_locs$text))

    batch_resp = batch_osm_query(query_locs)

    # write out the frequncies of organizations, 
    # we want to make sure that the top ones are correct
    freq_locs = as.data.frame(table(all_missing_locs$text))
    colnames(freq_locs)[1] = "query"

    cache_file = file.path(ref_data_dir, "/osm_cache.tsv")
    cache_df = data.frame(fread(cache_file))
    freq_locs = merge(freq_locs, cache_df)
    freq_locs = freq_locs[order(freq_locs$Freq, decreasing=T),]
    freq_locs$hand_edited = F

    cache_edit_file = file.path(ref_data_dir, "/osm_cache_edited2.tsv")
    write.table(freq_locs, cache_edit_file, sep="\t", quote=F, row.names=F)


}


#' private method that updates the gender cache. 
#' This method should not be run by users since they 
#' should always have a fully populated cache.
#' 
#' @param in_dir, full location of folder that contains all quote_table_raw tables
#' from pipeline step 4. Files must contain the string missed_generize_io_names
#' in the name
query_genderize_io <- function(in_dir) {

    # this method will just populate the cache with any locations
    # that are missing

    # people should never use this method, this is only
    # for me to run once in a while if I scraped new articles

    # find all files that contain missed names
    missed_gender_files = list.files(in_dir, 
                                        pattern="missed_generize_io_names",
                                        recursive=T,
                                        full.names=T)
    if(length(missed_gender_files) == 0){
        warn_str = "No missing name files found. Are you supplying the correct input dir?"
        warning(warn_str)
        return()
    }

    
    # read them all in and make unique for batch query
    all_names = NA
    for(curr_file in missed_gender_files){
        in_df = data.frame(fread(curr_file))
        if(nrow(in_df) == 0){
            next
        }
        all_names = rbind(all_names, in_df)
    }
    all_names = all_names[-1,]
    all_names = unique(all_names)


    # make sure names weren't missed in the already existing genderize reference files
    reference_files = file.path(ref_data_dir, "/genderize.tsv")
    ref_df = data.frame(fread(reference_files))
    reference_files = file.path(ref_data_dir, "/genderize_update.tsv")
    ref_update_df = data.frame(fread(reference_files))
    ref_df = rbind(ref_df, ref_update_df)

    if(length(intersect(all_names, ref_df$fore_name_simple)) != 0){
        all_names = setdiff(all_names, ref_df$fore_name_simple)
        warn_str = "when making genderize.io query, 
                there were names that have already 
                been included in reference. 
                Re-run the quote-postprocessing, it is likely out of date."
        warning(warn_str)
    }
    if(length(all_names) == 0){
        return()
    }


    # now run the genderize.io request
    # be CARFEUL only 1000 a day!
    ## if gender is still unknown, make a best guess
    require("genderizeR")
    max_len = min(1000, length(all_names))
    query_names = sample(all_names)[1:max_len]
    names_processed = data.frame(findGivenNames(query_names, textPrepare=F))
    names_processed = unique(names_processed)
    names_processed$probability = as.numeric(names_processed$probability)
    names_processed_male = subset(names_processed, gender == "male")
    names_processed_female = subset(names_processed, gender == "female")
    names_processed_female$probability = 1 - names_processed_female$probability
    names_processed = rbind(names_processed_female, names_processed_male)

    

    # make it in the same format as the reference data
    colnames(names_processed) = c("fore_name_simple", "remove", "probability_male", "genderize_sample_size", "rm")
    names_processed$n_authors = NA
    names_processed$query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    names_processed = names_processed[,c("fore_name_simple", "n_authors", 
                            "genderize_sample_size", "query_date", 
                            "probability_male")]
    ref_update_df = rbind(ref_update_df, names_processed)
    ref_update_df = unique(ref_update_df)

    # get all the names that were not found, so we don't check them again
    missing_names = setdiff(query_names, names_processed$fore_name_simple)
    if(length(missing_names) > 0){
        missing_df = data.frame(fore_name_simple = missing_names,
                                n_authors = NA, genderize_sample_size = NA,
                                query_date = as.Date(Sys.Date(), format = "%B %d %Y"),
                                probability_male = NA)
        ref_update_df = rbind(ref_update_df, missing_df)
    }
    ref_update_df = unique(ref_update_df)


    # now write it out
    outfile = file.path(ref_data_dir, "/genderize_update.tsv")
    write.table(ref_update_df, outfile, sep="\t", quote=F, row.names=F)

    # now delete the files
    file.remove(missed_gender_files)
    
}
