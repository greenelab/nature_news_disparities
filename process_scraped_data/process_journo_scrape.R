library(jsonlite)
library(data.table)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
MIN_BODY_SIZE = 100


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

join_journo_ref <- function(journo_df, ref_df){

    # first pass
    # get all the journo entries that have a 
    # matching file id to the reference df
    same_ids = merge(journo_df[,c("file_id", "year", "authors")],
                        ref_df[,c("file_id", "year")])

    remaining_journo_ids = setdiff(journo_df$file_id, same_ids$file_id)
    remaining_ref_ids = setdiff(ref_df$file_id, same_ids$file_id)
    remaining_df = subset(journo_df, file_id %in% remaining_journo_ids)
    remaining_ref_df = subset(ref_df, file_id %in% remaining_ref_ids)


    if(nrow(remaining_df) == 0){
        same_ids$journo_file_id = same_ids$file_id
        return(same_ids)
    }

    # second pass -- replace news0 with 0, and remove .html
    remaining_df$file_id = gsub("news0", "0", remaining_df$file_id)
    remaining_ref_df$has_html = FALSE
    remaining_ref_df[grep(".html", remaining_ref_df$file_id, fixed=T), "has_html"] = TRUE
    remaining_ref_df$file_id = gsub(".html", "", remaining_ref_df$file_id, fixed=T)

    same_ids2 = merge(remaining_df[,c("file_id", "year", "authors")],
                        remaining_ref_df[,c("file_id", "year", "has_html")])


    remaining_journo_ids2 = setdiff(journo_df$file_id, same_ids2$file_id)
    remaining_ref_ids2 = setdiff(remaining_ref_df$file_id, same_ids2$file_id)
    remaining_df2 = subset(remaining_df, file_id %in% remaining_journo_ids2)
    remaining_ref_df2 = subset(remaining_ref_df, file_id %in% remaining_ref_ids2)

    remaining_df = remaining_df2
    remaining_ref_df = remaining_ref_df2

    same_ids2$file_id[which(same_ids2$has_html)] = paste0(same_ids2$file_id[which(same_ids2$has_html)], ".html")
    same_ids2 = same_ids2[,colnames(same_ids)]
    same_ids = rbind(same_ids, same_ids2)

    # second pass
    # try to get substring match using the body text
    # ref_df will have less text, so we use this as the query
    # it has less text because there was some changes to nature formatting
    # and now the scrapr returns subheaders

    if(nrow(remaining_df) == 0){
        same_ids$journo_file_id = same_ids$file_id
        return(same_ids)
    }

    remaining_df = remaining_df[,c("file_id", "year", "authors", "body")]
    remaining_ref_df = remaining_ref_df[,c("file_id", "year", "body")]

    colnames(remaining_df) = paste0("journo_", colnames(remaining_df))
    same_ids$journo_file_id = same_ids$file_id

    colnames(remaining_ref_df) = paste0("ref_", colnames(remaining_ref_df))

    # make additional column to fill
    # with the refernce info
    remaining_df$file_id = NA

    for(curr_ref_idx in 1:nrow(remaining_ref_df)){
        curr_ref = remaining_ref_df[curr_ref_idx,]

        if(nchar(curr_ref$ref_body) < MIN_BODY_SIZE){
            print("Body too short")
            print(curr_ref_idx)
            next
        }

        query_str = substring(curr_ref$ref_body, 1, MIN_BODY_SIZE)
        found_idx = grep(query_str, remaining_df$journo_body, fixed=TRUE)

        # we found an exact, single match
        # for a file that has not already been matched
         if (length(found_idx) == 0 ) {
            print("No match!")
            print(curr_ref_idx)
        }else if(length(found_idx) == 1 & is.na(remaining_df$file_id[found_idx])){
            remaining_df$file_id[found_idx] = curr_ref$ref_file_id
        }else if (length(found_idx) > 1) {
            print("More than one match for query string")
            print(curr_ref_idx)
        }else if (length(found_idx) == 1 & !is.na(remaining_df$file_id[found_idx])) {
            print("Already Found a match!")
            print(curr_ref_idx)
        }else{
            print("fall through")
            print(curr_ref_idx)
            return()
        }
    }

    colnames(remaining_df) = c("journo_file_id", "year", "authors", "body", "file_id")
    remaining_df = remaining_df[,colnames(same_ids)]
    remaining_df = rbind(same_ids, remaining_df)

    return(remaining_df)

}


### read in arguements
args = commandArgs(trailingOnly=TRUE)
journo_file = args[1]
ref_file = args[2]
out_file = args[3]

# format the json from scrapy
journo_df = read_json(journo_file)
journo_df = process_body(journo_df)

# read in the reference file
ref_df = read_json(ref_file)
ref_df = process_body(ref_df)

merge_df = join_journo_ref(journo_df, ref_df)
merge_df$authors <- unlist(lapply(merge_df$authors, function(x) paste(unlist(x), collapse = "; ")))

write.table(merge_df, out_file, sep="\t", quote=F, row.names=F)

