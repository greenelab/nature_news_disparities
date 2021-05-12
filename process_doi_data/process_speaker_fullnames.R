
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

    # format the output
    col_ids = c("year", "type", "author", "file_id", "quote")
    full_quote_df = unique(full_quote_df[,col_ids])

    all_speaker_file = file.path(outdir, "all_speaker_fullname.tsv")
    write.table(full_quote_df, file=all_speaker_file, sep="\t", quote=F, row.names=F)


}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
quote_dir = args[1]
outdir = args[2]

process_all_quote_fullnames(quote_dir, outdir)

