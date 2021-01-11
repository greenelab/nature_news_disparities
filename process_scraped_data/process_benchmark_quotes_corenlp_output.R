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
    tolower(in_df$canonical_speaker) %in% male_pronouns
    )

    in_df$gender[male_idx] = "MALE"

    female_idx = which((in_df$gender == "UNKNOWN" | is.na(in_df$gender) ) &
                        tolower(in_df$canonical_speaker) %in% female_pronouns)

    in_df$gender[female_idx] = "FEMALE"

    ## if gender is still unknown, make a best guess
    require("genderizeR")

    already_gendered_df = subset(in_df, gender!="UNKNOWN" | is.na(full_name))
    unknown_gendered_df = subset(in_df, gender=="UNKNOWN")

    names_missing = unique(unknown_gendered_df$full_name)
    names_processed = findGivenNames(names_missing)
    genderize_names = data.frame(genderize(names_missing, names_processed))
    genderize_names$guessed_gender = toupper(genderize_names$gender)
    colnames(genderize_names)[which(colnames(genderize_names)=="text")] = "full_name"

    unknown_gendered_df = merge(
       unknown_gendered_df, 
       genderize_names[,c("full_name", "guessed_gender")]
     )
    unknown_gendered_df$gender = unknown_gendered_df$guessed_gender
    unknown_gendered_df = unknown_gendered_df[,colnames(already_gendered_df)]

    full_gender_df = rbind(unknown_gendered_df, already_gendered_df)


    return(full_gender_df)

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

    all_quotes = all_quotes[,c(
      "file_id", "full_name", 
      "gender", "canonical_speaker", 
      "partial_name", "quote"
    )]
    return(all_quotes)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outfile = args[2]

quote_res = read_result_files(corenlp_output_dir)
write.table(quote_res, file=outfile, sep="\t", quote=F, row.names=F)
