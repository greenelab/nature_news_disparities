require(jsonlite)
require(data.table)
require(here)

proj_dir = here()
source(paste(proj_dir, "/utils/scraper_processing_utils.R", sep=""))


format_gender_canonical_speaker <- function(in_df){


    # if the gender is unknown, but there is a 
    # gender guess in canonical speaker, take it

    male_pronouns = na.omit(pronouns_df$pronouns[pronouns_df$gender == "MALE"])
    female_pronouns = na.omit(pronouns_df$pronouns[pronouns_df$gender == "FEMALE"])

    male_idx = which(
        (in_df$gender == "UNKNOWN" | is.na(in_df$gender)) &
        (tolower(in_df$canonical_speaker) %in% male_pronouns | 
        tolower(in_df$partial_name) %in% male_pronouns)
    )

    in_df$gender[male_idx] = "MALE"

    female_idx = which(
        (in_df$gender == "UNKNOWN" | is.na(in_df$gender) ) &
        (tolower(in_df$canonical_speaker) %in% female_pronouns |
        tolower(in_df$partial_name) %in% female_pronouns)
    )


    in_df$gender[female_idx] = "FEMALE"

    ## if gender is still unknown, make a best guess using genderizer
    ## with reference data
    already_gendered_df = subset(in_df, gender!="UNKNOWN" | is.na(full_name))
    unknown_gendered_df = subset(in_df, gender=="UNKNOWN" | is.na(gender))

    # get first name to check the gender
    unknown_gendered_df$first_name = sapply(strsplit(unknown_gendered_df$full_name," "), `[`, 1)
    unknown_gendered_df$first_name = tolower(unknown_gendered_df$first_name)

    names_missing = data.frame(first_name=unique(unknown_gendered_df$first_name))

    gender_io_file = paste(proj_dir, "/data/reference_data/genderize.tsv", sep="")
    names_processed = data.frame(fread(gender_io_file))
    gender_io_file = paste(proj_dir, "/data/reference_data/genderize_update.tsv", sep="")
    names_processed = rbind(names_processed, data.frame(fread(gender_io_file)))

    colnames(names_processed)[1] = "first_name"
    names_processed$guessed_gender = "MALE"
    names_processed$guessed_gender[names_processed$probability_male < 0.5] = "FEMALE"

    # guess genders from reference
    names_processed = merge(data.table(names_missing), 
                            data.table(names_processed),
                            all.x=T)

    names_processed = data.frame(names_processed)

    # save these to add to the reference dataset later
    names_not_processed = names_processed$first_name[is.na(names_processed$query_date)]

    unknown_gendered_df = merge(
       unknown_gendered_df, 
       names_processed[,c("first_name", "guessed_gender")]
     )
    unknown_gendered_df$gender = unknown_gendered_df$guessed_gender
    unknown_gendered_df = unknown_gendered_df[,colnames(already_gendered_df)]

    full_gender_df = rbind(unknown_gendered_df, already_gendered_df)


    return(list(full_gender_df, names_not_processed))

}

read_result_files <- function(corenlp_output_dir){
    
    json_res_files = list.files(corenlp_output_dir, pattern=".txt.json", full.names = TRUE)
    
    all_quotes = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        ## get quotes
        quotes_res = json_res$quotes
        if(length(quotes_res) == 0){
            print("no quote")
            next
        }
        
        quotes_res = quotes_res[,c("text", "speaker", "canonicalSpeaker")]
        colnames(quotes_res) = c("quote", "partial_name", "canonical_speaker")
        quotes_res$file_id = file_id

        ## get all person info
        print(file_id)
        speaker_df = get_persons(json_res)

        ## format final df
        if(! all(is.na(speaker_df))){
            quotes_res = merge(quotes_res, speaker_df, 
                                all.x=T, by="partial_name") 
            # if there was no full name matching a speaker
            # replace with with the canonical_speaker
            idx_no_name = is.na(quotes_res$full_name)
            quotes_res$full_name[idx_no_name] = 
                quotes_res$canonical_speaker[idx_no_name]
            quotes_res$gender[idx_no_name] = "UNKNOWN"

        }else{
            # no speakers found, so make it NA
            quotes_res$full_name = NA
            quotes_res$gender = "UNKNOWN"

        }
        all_quotes = rbind(all_quotes, quotes_res)

    }

    all_quotes = all_quotes[-1,]

    # format gender is canonical speaker has an idea
    res = format_gender_canonical_speaker(all_quotes)

    all_quotes_gender = res[[1]]
    all_quotes_gender = all_quotes_gender[,c(
      "file_id", "full_name", 
      "gender", "canonical_speaker", 
      "partial_name", "quote"
    )]

    # write out any names that were not in the reference for batch update later
    unprocessed_names_gender = res[[2]]
    outfile = paste(corenlp_output_dir, "/missed_generize_io_names.tsv", sep="")
    if(file.exists(outfile)){
        prev_unprocessed = data.frame(fread(outfile))
        unprocessed_names_gender = rbind(unprocessed_names_gender, prev_unprocessed)
        unprocessed_names_gender = unique(na.omit(unprocessed_names_gender))
    }
    write.table(unprocessed_names_gender, file=outfile, sep="\t", quote=F, row.names=F)



    return(all_quotes_gender)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outfile = args[2]

quote_res = read_result_files(corenlp_output_dir)
write.table(quote_res, file=outfile, sep="\t", quote=F, row.names=F)
