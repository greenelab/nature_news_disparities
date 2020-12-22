require(data.table)
require(tidyr)
require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/reference_data/", sep = "")


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


    # remove .
    format_str = gsub(".", "", format_str, fixed=T)

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
    lcs_idx = amatch(tolower(key_str), tolower(str_vec), maxDist=Inf, method="lcs")
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

get_ref_location_file <- function(){

    ref_file = paste(ref_data_dir, "nature_index_export.csv", sep="")
    if(!file.exists(ref_file)){
        stop("nature_index_export file not found, please run setup.sh to get it")
    }

    ref_df = data.frame(fread(ref_file))

    # now split abbr
    ref_split_df = separate(data = ref_df, col = Institution, into = c("Inst", "abbr"), sep = " \\(")
    ref_split_df$abbr = gsub(")", "", ref_split_df$abbr)
    return(ref_split_df)
    
}

format_country_name <- function(ref_df, in_df){

    if(!"country" %in% colnames(in_df)){
        stop("input data frame must have a column named country")
    }


    full_loc_name = unique(unlist(ref_df$Country))
    in_df$location = NA
    for(idx in 1:nrow(in_df)){
        curr_text = unlist(in_df$country[idx])

        matched_country = grep(curr_text, full_loc_name, fixed = TRUE, value=T)
        if(length(matched_country) == 0){
            in_df$location[idx] = curr_text
        }else if(length(matched_country) > 1){
            in_df$location[idx] = "NOT_CLEAR"
        }else{
            in_df$location[idx] = matched_country
        }
        
    }

    return(in_df)

}


get_country_state_info <- function(){

    ref_org_df = get_ref_location_file()

    country_file = paste(ref_data_dir, "cdh_country_codes.txt", sep="")
    if(!file.exists(country_file)){
        stop("cdh_country_codes file not found, please run setup.sh to get it")
    }

    state_file = paste(ref_data_dir, "cdh_state_codes.txt", sep="")
    if(!file.exists(state_file)){
        stop("cdh_country_codes file not found, please run setup.sh to get it")
    }

    # format country file
    country_df = data.frame(fread(country_file))
    colnames(country_df)[which(colnames(country_df) == "FIPS.COUNTRY.NAME")] = "country"
    colnames(country_df)[which(colnames(country_df) == "UN.REGION")] = "un_region"
    colnames(country_df)[which(colnames(country_df) == "UN.SUB.REGION.NAME")] = "un_subregion"
    country_df = country_df[,c("country", "un_region", "un_subregion")]

    # format state file
    state_df = data.frame(fread(state_file))
    colnames(state_df)[which(colnames(state_df) == "V1")] = "country"
    colnames(state_df)[which(colnames(state_df) == "SUBDIVISION.STATE.ALTERNATE.NAMES")] = "province_state_alt"
    colnames(state_df)[which(colnames(state_df) == "ISO.3166.2.SUBDIVISION.STATE.NAME")] = "province_state"
    state_df = state_df[,c("country", "province_state_alt", "province_state")]

    # now we have a location annotation file
    full_loc_df = merge(country_df, state_df, by="country")

    #make all province_state options into long form table
    require("stringr")
    max_num_names = grep(", ", full_loc_df$province_state_alt, value=T)
    max_num_names = max(unlist(lapply(max_num_names, str_count, ",")))
    into_vec = paste("vers", 1:max_num_names, sep="_")
    full_loc_df = separate(data = full_loc_df, col = province_state, sep = ", ", into=into_vec)
    full_loc_df = melt(full_loc_df, id.vars=c("country", "un_region", "un_subregion"))
    full_loc_df = subset(full_loc_df, variable != "province_state_alt")
    full_loc_df = full_loc_df[,c("country", "un_region", "un_subregion", "value")]
    colnames(full_loc_df)[4] = "province_state"
    full_loc_df = unique(full_loc_df)


    # format the country name to match the organization style
    full_loc_df_formatted = format_country_name(ref_org_df, full_loc_df)


    return(full_loc_df_formatted[,2:5])


}

get_org_country_inner <- function(org_name, ref_df){

    substr_idx = which(grepl(tolower(org_name), tolower(ref_df$Inst), fixed=T))
    country_found = unique(ref_df$Country[substr_idx])

    if(length(country_found) > 1 ){
        country_found = "NOT_CLEAR"
    }

    if(length(country_found) == 0 ){
        country_found = "NOT_FOUND"
    }

    return(unique(country_found))

}

get_org_country <- function(ref_df, all_orgs){

    return(lapply(all_orgs, get_org_country_inner, ref_df))


}


get_prov_country_inner <- function(prov_name, ref_df){

    substr_idx = which(grepl(tolower(prov_name), tolower(ref_df$province_state), fixed=T))
    country_found = unique(ref_df$location[substr_idx])

    if(length(country_found) > 1 ){
        country_found = "NOT_CLEAR"
    }

    if(length(country_found) == 0 ){
        country_found = "NOT_FOUND"
    }

    return(unique(country_found))

}

get_prov_country <- function(ref_df, all_prov){

    return(lapply(all_prov, get_prov_country_inner, ref_df))


}


get_locations <- function(json_res){

    # get named entities --locations
    ner_df = get_ner(json_res)

    ner_locs_df = subset(ner_df, ner %in% c("ORGANIZATION"))
    ner_locs_df = unique(ner_locs_df)

    ref_df = get_ref_location_file()
    ner_locs_df$country = get_org_country(ref_df, ner_locs_df$text)
    ner_locs_df$location = ner_locs_df$country

    # accumulator
    locs_df = NA

    ### now do COUNTRY DISABIGUATION
    ner_country_df = subset(ner_df, ner %in% c("COUNTRY"))
    if(nrow(ner_country_df) != 0){

        ner_country_df$country = ner_country_df$text
        ner_country_df = format_country_name(ref_df, ner_country_df)

        locs_df = rbind(ner_locs_df, ner_country_df)

    }

    ### now do state_province disambiguation 
    ner_province_df = subset(ner_df, ner %in% c("STATE_OR_PROVINCE"))
    ref_provence_df = get_country_state_info()
    if(nrow(ner_province_df) != 0){

        ner_province_df$province_state = ner_province_df$text
        ner_province_df$location = unlist(get_prov_country(ref_provence_df, ner_province_df$province_state))
        ner_province_df$country = ner_province_df$location
        ner_province_df = ner_province_df[,colnames(locs_df)]

        if(is.na(locs_df)){
            locs_df = rbind(ner_locs_df, ner_province_df)
        }else{
            locs_df = rbind(locs_df, ner_province_df)
        }

    }

    locs_df$location = unlist(locs_df$location)
    locs_df = unique(locs_df)

    locs_df = merge(locs_df, ref_provence_df[,c("location", "un_region", "un_subregion")], all.x=T)
    locs_df = unique(locs_df)

    # post process for consistency
    locs_df$un_region[locs_df$location != "NOT_FOUND" & 
                        locs_df$location != "NOT_CLEAR" &
                        is.na(locs_df$un_region)] = 
                        
                        locs_df$country[locs_df$location != "NOT_FOUND" & 
                        locs_df$location != "NOT_CLEAR" &
                        is.na(locs_df$un_region)]

    locs_df$un_subregion[locs_df$location != "NOT_FOUND" & 
                        locs_df$location != "NOT_CLEAR" &
                        is.na(locs_df$un_subregion)] = 
                        
                        locs_df$country[locs_df$location != "NOT_FOUND" & 
                        locs_df$location != "NOT_CLEAR" &
                        is.na(locs_df$un_subregion)]

    locs_df$un_region[locs_df$location == "NOT_FOUND" | locs_df$location == "NOT_CLEAR"] = NA
    locs_df$un_subregion[locs_df$location == "NOT_FOUND" | locs_df$location == "NOT_CLEAR"] = NA

 
    return(locs_df)


}

get_persons <- function(json_res){

    # get named entities --names
    ner_df = get_ner(json_res)

    ner_names_df = subset(ner_df, ner=="PERSON")
    ner_names_df = subset(ner_names_df, !tolower(text) %in% pronouns)
    ner_names_df = data.frame(text=ner_names_df$text, gender="UNKNOWN")


    # get coreferenced names for later merging
    coref_list = json_res$corefs
    coref_df = do.call(rbind, coref_list)
    coref_names_df = subset(coref_df, type=="PROPER" & 
                                animacy == "ANIMATE" & 
                                number == "SINGULAR")

    coref_names_df = coref_names_df[,c("text", "gender")]

    # if there are no names, return NA
    if(nrow(ner_names_df) == 0){
        print("no speakers found")
        return(NA)
    }

    # now find the longest strings and gender
    max_str = c()
    for(curr_text in ner_names_df$text){

        curr_text = format_name_str(curr_text)

        str_idx = grep(curr_text, max_str)
        if(length(str_idx) == 0){
            # string not found, add it
            max_str = c(max_str, curr_text)
        }
        else if(length(str_idx) > 1){
            # there is an error, substring found twice
            print("error in parsing the speakers")
            print(curr_text)
            return(NA)
        }else{
            old_str = max_str[str_idx]
            if(nchar(old_str) < nchar(curr_text)){
                max_str[str_idx] = curr_text
            }
        }
    }

    # now convert names
    names_df = rbind(ner_names_df, coref_names_df)
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
