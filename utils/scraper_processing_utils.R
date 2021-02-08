require(data.table)
require(tidyr)
require(here)

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

}

get_matched_string <- function(key_str, str_vec){
    require("stringdist")

    # get match via longest common substring
    # where deletions are NOT penalized
    costs = sapply(tolower(str_vec), adist, y=tolower(key_str), costs=list(deletions=0))
    lcs_idx = which(costs == min(costs)[1])
    return(str_vec[lcs_idx])

}

get_ner <- function(json_res){

    # get named entities
    ner_list = json_res$sentences$entitymentions
    ner_list = ner_list[lapply(ner_list,length)>0]
    ner_list = lapply(ner_list, function(x) x[,c("text", "ner")])
    ner_df = do.call(rbind, ner_list)

    return(ner_df)
}

get_country_info <- function(){

    # now get the UN region info
    country_file = paste(ref_data_dir, "cdh_country_codes.txt", sep="")
    if(!file.exists(country_file)){
        stop("cdh_country_codes file not found, please run setup.sh to get it")
    }

    # format country file
    country_df = data.frame(fread(country_file))
    colnames(country_df)[which(colnames(country_df) == "FIPS.COUNTRY.NAME")] = "country"
    colnames(country_df)[which(colnames(country_df) == "ISO.3166.1.COUNTRY.CHAR.2.CODE")] = "address.country_code"
    colnames(country_df)[which(colnames(country_df) == "UN.REGION")] = "un_region"
    colnames(country_df)[which(colnames(country_df) == "UN.SUB.REGION.NAME")] = "un_subregion"
    country_df = country_df[,c( "country", "address.country_code", "un_region", "un_subregion")]
    country_df$address.country_code = tolower(country_df$address.country_code)

    return(country_df)
}

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

get_persons <- function(json_res){

    # get named entities --names
    ner_df = get_ner(json_res)

    ner_names_df = subset(ner_df, ner=="PERSON")
    ner_names_df = subset(ner_names_df, !tolower(text) %in% pronouns)

   # if there are no names, return NA
    if(nrow(ner_names_df) == 0){
        print("no speakers found")
        return(NA)
    }
    ner_names_df = data.frame(text=ner_names_df$text, gender="UNKNOWN")


    # get coreferenced names for later merging
    coref_list = json_res$corefs
    coref_df = do.call(rbind, coref_list)
    coref_names_df = subset(coref_df, type=="PROPER" & 
                                animacy == "ANIMATE" & 
                                number == "SINGULAR")

    coref_names_df = coref_names_df[,c("text", "gender")]
    if(nrow(coref_names_df) != 0){
        coref_names_df$gender = "UNKNOWN"
    }

 
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
    names_df = rbind(ner_names_df, coref_names_df)
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


single_osm_query <- function(curr_q){

    # we need to follow OSM quidelines
    # we MUST cache
    # no parallel processes
    # no more than one query per second

    require(tmaptools)

    # make sure query is not in cache
    cache_file = paste(ref_data_dir, "/osm_cache.tsv", sep="")
    if(!file.exists(cache_file)){
        stop("osm_cache file not found, file should be in git, 
                you must download this before querying locations")
    }

    cache_df = data.frame(fread(cache_file))
    if(tolower(curr_q) %in% tolower(cache_df$query)){
        return(subset(cache_df, tolower(query) == tolower(curr_q)))
    }
    
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
    cache_file = paste(ref_data_dir, "/osm_cache.tsv", sep="")
    write.table(cache_df, cache_file, sep="\t", quote=F, row.names=F)

    Sys.sleep(1)

    return(resp_df)


}

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

initial_osm_query <- function(in_dir) {

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

    # read in location files
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

    cache_file = paste(ref_data_dir, "/osm_cache.tsv", sep="")
    cache_df = data.frame(fread(cache_file))
    freq_locs = merge(freq_locs, cache_df)
    freq_locs = freq_locs[order(freq_locs$Freq, decreasing=T),]
    freq_locs$hand_edited = F

    cache_edit_file = paste(ref_data_dir, "/osm_cache_edited2.tsv", sep="")
    write.table(freq_locs, cache_edit_file, sep="\t", quote=F, row.names=F)


}

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
        all_names = rbind(all_names, in_df)
    }
    all_names = all_names[-1,]
    all_names = unique(all_names)


    # make sure names weren't missed in the already existing genderize reference files
    reference_files = paste(ref_data_dir, "/genderize.tsv", sep="")
    ref_df = data.frame(fread(reference_files))
    reference_files = paste(ref_data_dir, "/genderize_update.tsv", sep="")
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
    names_processed = data.frame(findGivenNames(all_names))
    names_processed = unique(names_processed)
    

    # make it in the same format as the reference data
    colnames(names_processed) = c("fore_name_simple", "remove", "probability_male", "genderize_sample_size", "rm")
    names_processed$n_authors = NA
    names_processed$query_date = as.Date(Sys.Date(), format = "%B %d %Y")
    names_processed = names_processed[,c("fore_name_simple", "n_authors", 
                            "genderize_sample_size", "query_date", 
                            "probability_male")]
    ref_update_df = rbind(ref_update_df, names_processed)
    ref_update_df = unique(ref_update_df)

    # now write it out
    outfile = paste(ref_data_dir, "/genderize_update.tsv", sep="")
    write.table(ref_update_df, outfile, sep="\t", quote=F, row.names=F)

    # now delete the files
    file.remove(missed_gender_files)
    
}