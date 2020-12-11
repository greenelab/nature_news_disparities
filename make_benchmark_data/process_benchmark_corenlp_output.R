library(jsonlite)
library(data.table)

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
    library("stringr")

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
    library("stringdist")

    # get match via longest common substring
    lcs_idx = amatch(key_str, str_vec, maxDist=Inf, method="lcs")
    return(str_vec[lcs_idx])

}

get_speakers <- function(json_res){

    # get named entities names
    ner_list = json_res$sentences$entitymentions
    ner_list = ner_list[lapply(ner_list,length)>0]
    ner_list = lapply(ner_list, function(x) x[,c("text", "ner")])
    ner_df = do.call(rbind, ner_list)

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

format_gender_canonical_speaker <- function(in_df){

    # if the gender is unknown, but there is a 
    # gender guess in canonical speaker, take it

    male_pronouns = na.omit(pronouns_df$pronouns[pronouns_df$gender == "MALE"])
    female_pronouns = na.omit(pronouns_df$pronouns[pronouns_df$gender == "FEMALE"])

    male_idx = which((in_df$gender == "UNKNOWN" | is.na(in_df$gender)) &
                        tolower(in_df$canonical_speaker) %in% male_pronouns)

    in_df$gender[male_idx] = "MALE"

    female_idx = which((in_df$gender == "UNKNOWN" | is.na(in_df$gender) ) &
                        tolower(in_df$canonical_speaker) %in% female_pronouns)

    in_df$gender[female_idx] = "FEMALE"

    return(in_df)

}

read_result_files <- function(corenlp_output_dir){
    
    json_res_files = list.files(corenlp_output_dir, pattern=".txt.json", full.names = TRUE)
    
    all_quotes = NA
    for(curr_file in json_res_files){
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        ## get quotes
        quotes_res = json_res$quotes
        if(length(quotes_res) == 0){
            next
        }
        
        quotes_res = quotes_res[,c("text", "speaker", "canonicalSpeaker")]
        colnames(quotes_res) = c("quote", "partial_name", "canonical_speaker")
        quotes_res$file_id = file_id

        ## get speaker info
        print(file_id)
        speaker_df = get_speakers(json_res)

        ## format final df
        if(!is.na(speaker_df)){
            quotes_res = merge(quotes_res, speaker_df, 
                                all.x=T, by="partial_name") 

        }else{
            # no speakers found, so make it NA
            quotes_res$full_name = NA
            quotes_res$gender = "UNKNOWN"

        }
        all_quotes = rbind(all_quotes, quotes_res)

    }

    all_quotes = all_quotes[-1,]

    # format gender is canonical speaker has an idea
    all_quotes = format_gender_canonical_speaker(all_quotes)

    all_quotes = all_quotes[,c("file_id", "full_name", "gender", "canonical_speaker", "partial_name", "quote")]
    return(all_quotes)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outfile = args[2]

quote_res = read_result_files(corenlp_output_dir)
write.table(quote_res, file=outfile, sep="\t", quote=F, row.names=F)
