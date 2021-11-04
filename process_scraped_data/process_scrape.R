library(jsonlite)
library(data.table)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))

#' Format the body of the article to remove 
#' have a unified encoding
#' 
#' @param in_df, data frame with a column "body"
#' that contains all the text from the article.
#' Each row is assumed to be an article
#' 
#' @return in_df, data frame where the "body" column is now formatted
process_body <- function(in_df){

    library(stringr)
    # remove redundant white space
    in_df$body = str_squish(in_df$body)

    # convert unicode stuff
    ## TODO: Do this for all languages, JIC
    in_df$body = gsub("\u2019|\u2018|‘|’", "'", in_df$body)
 
    in_df$body = gsub("\u201c|\u201d|“|”|\u0022", "\"", in_df$body)

    in_df$body = gsub("\u2014", "—", in_df$body)

    in_df$body = gsub("\u00a0", " ", in_df$body)

    return(in_df)

}

#' For coreNLP each article needs to be in its own file
#' and we have to make a reference file (file_list.txt) containing all the 
#' file names we would like coreNLP to look over
#' 
#' @param in_df, data frame with a column "body"
#' that contains all the text from the article.
#' Each row is assumed to be an article
#' @param outdir, directory path where the files should be written
write_sep_files <- function(in_df, outdir){

    # for easier processing with coreNLP write out the files
    # individually
    all_files = c()
    for(curr_article_idx in 1:nrow(in_df)){
        curr_article = in_df[curr_article_idx,]
        outfile = file.path(outdir, paste(curr_article$file_id, ".txt", sep=""))
        article_body = unlist(curr_article$body)
        if(nchar(article_body) == 0){
            next
        }
        writeLines(article_body, outfile)
        all_files = paste(all_files, outfile, "\n", sep="")
    }

    outfile = file.path(outdir, "file_list.txt")
    writeLines(all_files, outfile)

}

### read in arguements
args = commandArgs(trailingOnly=TRUE)
infile = args[1]
outdir = args[2]

# format the json from scrapy
bm_data_df = read_json(infile)
bm_data_df = process_body(bm_data_df)

# write out the files for coreNLP
set.seed(5)
bm_data_df = bm_data_df[sample(nrow(bm_data_df)),]


write_sep_files(bm_data_df, outdir)
id_year_file = file.path(outdir, "/fileID_year.tsv")
write.table(bm_data_df[,c("file_id", "year")], id_year_file, sep="\t", quote=F, row.names=F)
