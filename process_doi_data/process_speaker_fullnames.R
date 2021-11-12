
require(jsonlite)
require(data.table)
require(stringr)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))
source(file.path(proj_dir, "/analysis_scripts/analysis_utils.R"))



#' Read all json files to do last name origin prediction
#' of quoted speakers
#' @param quote_dir, directory containing scraped nature JSON output 
#' @param outdir, directory to put the results file 
#' 
process_all_quote_fullnames <- function(quote_dir, outdir){

    # read in the quotes from every year and every article type
    full_quote_df = NA
    quote_files = list.files(quote_dir, full.names = T)
    quote_files = grep("quote_table_raw_", quote_files, value=T)
    for(quote_file in quote_files){
        
        quote_df = read_corenlp_quote_files(quote_file)
        quote_df$year = str_extract(quote_file, "[1-9][0-9]+")
        quote_df$type = substring(basename(quote_file), 
                                22, nchar(basename(quote_file))-4)
        
        full_quote_df = rbind(full_quote_df, quote_df)
    }
    full_quote_df = full_quote_df[-1,]

    # now that we have the quotes with their names
    # we have to process the names such that we are sure we have 
    # the last name

    # first we remove anything that is not a full name
    # i.e. there must be a space
    space_name_idx = grep(" ", full_quote_df$est_speaker)
    full_quote_df = full_quote_df[space_name_idx, ]

    # now format the author name
    full_quote_df$author = format_author_fullname(full_quote_df$est_speaker)
    space_idx = grep(" ", full_quote_df$author)
    full_quote_df = full_quote_df[space_idx, ]

    # make sure its not an empty string
    non_whitespace = which(trimws(full_quote_df$author)!="")
    full_quote_df = full_quote_df[non_whitespace,]


    # format the output
    col_ids = c("year", "type", "author", "file_id", "quote")
    full_quote_df = unique(full_quote_df[,col_ids])

    all_speaker_file = file.path(outdir, "all_speaker_fullname.tsv")
    write.table(full_quote_df, file=all_speaker_file, sep="\t", quote=F, row.names=F)


}


#' Read in coreNLP speaker information and query the gender information
#' in the genderize.io cache. All names not found in the cache will 
#' be written to a file for later querying in corenlp_output_dir
#' named "missed_generize_io_names"
#' 
#' @param corenlp_output_dir, directory containing coreNLP JSON output 
#' 
#' @return dataframe, all_quotes_gender all gender information from speakers
process_all_mentioned_fullnames <- function(corenlp_output_dir, outdir){
  
    json_res_files = list.files(corenlp_output_dir, full.names = TRUE, recursive=T)
    json_res_files = grep("coreNLP_output", json_res_files, value=T)
    json_res_files = grep(".txt.json", json_res_files, value=T)
    
    all_persons = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        # get metadata
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)

        # get year
        folder_name = basename(dirname(curr_file))
        file_name_year = substring(basename(folder_name), 
                            16, 19)
    
        # get the news article type from the file name
        file_name_type = substring(basename(folder_name), 
                                21, nchar(basename(folder_name)))
            
        json_res = fromJSON(curr_file)

        ## get all person info
        print(file_id)
        speaker_df = get_persons(json_res)
        if(is.na(speaker_df)){
            next
        }

        speaker_df$year = file_name_year
        speaker_df$type = file_name_type
        speaker_df$file_id = file_id
        speaker_df = speaker_df[,c("year", "type", "full_name", "file_id")]
        colnames(speaker_df)[3] = "author"
        speaker_df = unique(speaker_df)

        all_persons = rbind(all_persons, speaker_df)

    }
    all_persons = all_persons[-1,]

    # only keep the name if there are atleast 2 parts
    # this means there must be a space
    space_idx = grep(" ", all_persons$author)
    all_persons = all_persons[space_idx, ]


    # now format the author name
    all_persons$author = format_author_fullname(all_persons$author)
    all_persons = subset(all_persons, author != "")
    all_persons = subset(all_persons, author != " ")
    space_idx = grep(" ", all_persons$author)
    all_persons = all_persons[space_idx, ]

    # make sure its not an empty string
    non_whitespace = which(trimws(all_persons$author)!="")
    all_persons = all_persons[non_whitespace,]


    outfile = file.path(outdir, "all_mentioned_fullname.tsv")
    write.table(all_persons, file=outfile, sep="\t", quote=F, row.names=F)


}


### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outdir = args[2]

process_all_quote_fullnames(corenlp_output_dir, outdir)
process_all_mentioned_fullnames(corenlp_output_dir, outdir)

