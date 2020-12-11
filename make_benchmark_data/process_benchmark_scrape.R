library(jsonlite)
library(data.table)


read_json <- function(infile){

    json_res = fromJSON(infile)
    json_res = data.frame(json_res)
    return(json_res)

}

process_body <- function(in_df){

    library(stringr)
    # remove redundant white space
    in_df$body = str_squish(in_df$body)

    # convert unicode stuff
    ## TODO: Do this for all languages, JIC
    in_df$body = gsub("\u2019", "'", in_df$body)
    in_df$body = gsub("\u2018", "'", in_df$body)
    in_df$body = gsub("‘", "'", in_df$body)
    in_df$body = gsub("’", "'", in_df$body)

    in_df$body = gsub("\u201c", "\"", in_df$body)
    in_df$body = gsub("“", "\"", in_df$body)

    in_df$body = gsub("\u201d", "\"", in_df$body)
    in_df$body = gsub("”", "\"", in_df$body)

    in_df$body = gsub("\u2014", "—", in_df$body)

    in_df$body = gsub("\u00a0", " ", in_df$body)

    in_df$body = gsub("\u0022", "\"", in_df$body)

    return(in_df)

}

write_sep_files <- function(in_df, out_dir){

    # for easier processing with coreNLP write out the files
    # individually
    all_files = c()
    for(curr_article_idx in 1:nrow(in_df)){
        curr_article = in_df[curr_article_idx,]
        outfile = paste(outdir, curr_article$file_id, ".txt", sep="")
        writeLines(unlist(curr_article$body), outfile)
        all_files = paste(all_files, outfile, "\n", sep="")
    }

    outfile = paste(outdir, "file_list.txt", sep="")
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
write_sep_files(bm_data_df, outdir)
id_year_file = paste(outdir, "/fileID_year.tsv", sep="")
write.table(bm_data_df[,c("file_id", "year")], id_year_file, sep="\t", quote=F, row.names=F)
