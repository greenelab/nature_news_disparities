
require(jsonlite)
require(data.table)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scrape_utils.R"))

get_author_gender <- function(unknown_gendered_df){

    unknown_gendered_df$author = tolower(unknown_gendered_df$author)
    names_missing = data.frame(author=unique(unknown_gendered_df$author))

    gender_io_file = file.path(proj_dir, "/data/reference_data/genderize.tsv")
    names_processed = data.frame(fread(gender_io_file))
    gender_io_file = file.path(proj_dir, "/data/reference_data/genderize_update.tsv")
    names_processed = rbind(names_processed, data.frame(fread(gender_io_file)))

    colnames(names_processed)[1] = "author"
    names_processed$guessed_gender = "MALE"
    names_processed$guessed_gender[names_processed$probability_male < 0.5] = "FEMALE"

    # guess genders from reference
    names_processed = merge(data.table(names_missing), 
                            data.table(names_processed),
                            all.x=T)

    names_processed = data.frame(unique(names_processed))

    # save these to add to the reference dataset later
    names_not_processed = names_processed$author[is.na(names_processed$query_date)]

    unknown_gendered_df = merge(unknown_gendered_df, 
                                unique(names_processed[,c("author", "guessed_gender")]),
                                all.x=T)
    unknown_gendered_df$gender = unknown_gendered_df$guessed_gender

    return(list(names_not_processed, unknown_gendered_df))

}


format_author_names <- function(author_vec, is_nature){

    if(is_nature){
        author_vec = unlist(lapply(author_vec, function(x) unlist(str_split(x, " "))[1]))
    }else{
        author_vec = unlist(lapply(author_vec, function(x) rev(unlist(str_split(x, ", ")))[1]))
    }
    author_vec = str_replace(author_vec, ",", "")
    author_vec = unlist(lapply(author_vec, format_name_str))


    # now remove anything that looks like a consortium or not a name
    non_name_check = "consortium|group|initiative|team|collab|committee|center|program|author|institute"
    non_name_idx = grep(non_name_check, author_vec)
    author_vec[non_name_idx] = ""

    # make sure we are only getting the first name
    author_vec = unlist(lapply(author_vec, function(x) unlist(str_split(x, " "))[1]))


    return(author_vec)
}

#' Read all nature json files and get author info
#' @param author_df, data.frame with all author info 
#' 
#' @return dataframe, reducing all author info to first and last authors only
format_authors <- function(author_df, is_nature=T){

    library(stringr)

    first_authors = unlist(lapply(author_df$authors, function(x) unlist(str_split(x, "; "))[1]))
    first_authors = format_author_names(first_authors, is_nature)

    last_authors = unlist(lapply(author_df$authors, function(x) rev(unlist(str_split(x, "; ")))[1]))
    last_authors = format_author_names(last_authors, is_nature)

    first_author_df = data.frame(doi = author_df$doi,
                                year = author_df$year,
                                author_pos = "first",
                                author = first_authors)
    last_author_df = data.frame(doi = author_df$doi,
                                year = author_df$year,
                                author_pos = "last",
                                author = last_authors)

    author_df = rbind(first_author_df, last_author_df)

    # if only one author counted twice.... needs a fix

    author_df = unique(author_df)
    return(author_df)

}


#' Read all nature json files and get author info
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
#' @return dataframe, all_authors author infos for nature articles
read_nature_author_json_files <- function(nature_dir){

    json_res_files = list.files(nature_dir, pattern=".json", full.names = TRUE)
    
    all_authors = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        # format authors
        authors = unlist(lapply(json_res$authors, function(x) paste(unlist(x$name), collapse="; ")))

        # make df
        authors_df = data.frame(file_id=json_res$file_id,
                                year=json_res$year,
                                authors=authors)
        
        all_authors = rbind(all_authors, authors_df)

    }

    all_authors = all_authors[-1,]

    # format file_id into a doi
    all_authors$doi = paste("doi:10.1038/", all_authors$file_id, sep="")

    return(all_authors)

}


#' Read all json files to do the gender prediction
#' the nature background authors
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
process_all_author_gender <- function(nature_dir, cited_dois_dir, outdir){

    # we have 3 source files for author info
    # springer background authorship
    springer_author_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_bg_author_cache.tsv")
    springer_author_df = data.frame(fread(springer_author_file))

    # springer cited authorship
    cited_author_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_cited_author_cache.tsv")
    cited_author_df = data.frame(fread(cited_author_file))
    cited_author_df = subset(cited_author_df, !is.na(authors))
    cited_doi_df = get_ref_dois(cited_dois_dir)
    cited_author_df = merge(cited_author_df, cited_doi_df[,c("doi", "year")], by="doi", all.x=T)

    # then all the nature articles
    nature_author_df = read_nature_author_json_files(nature_dir)

    # now process them to get the first and last authors
    cited_author_df = format_authors(cited_author_df, is_nature=F)
    springer_author_df = format_authors(springer_author_df, is_nature=F)
    nature_author_df = format_authors(nature_author_df, is_nature=T)

    # remove any blank authors
    cited_author_df = subset(cited_author_df, author != "")
    springer_author_df = subset(springer_author_df, author != "")
    nature_author_df = subset(nature_author_df, author != "")

    # now get the genders
    res = get_author_gender(cited_author_df)
    missed_names = res[[1]]
    cited_author_df_gender = res[[2]]

    res = get_author_gender(springer_author_df)
    missed_names = unique(c(missed_names, res[[1]]))
    springer_author_df_gender = res[[2]]

    res = get_author_gender(nature_author_df)
    missed_names = unique(c(missed_names, res[[1]]))
    nature_author_df_gender = res[[2]]

    missing_gender_file = file.path(outdir, "missed_generize_io_names.tsv")
    write.table(missed_names, file=missing_gender_file, sep="\t", quote=F, row.names=F)

    cited_author_file = file.path(outdir, "cited_author_gender.tsv")
    write.table(cited_author_df_gender, file=cited_author_file, sep="\t", quote=F, row.names=F)

    springer_author_gender_file = file.path(outdir, "springer_author_gender.tsv")
    write.table(springer_author_df_gender, file=springer_author_gender_file, sep="\t", quote=F, row.names=F)

    nature_author_gender_file = file.path(outdir, "nature_author_gender.tsv")
    write.table(nature_author_df_gender, file=nature_author_gender_file, sep="\t", quote=F, row.names=F)




}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
nature_dir = args[1]
cited_dois_dir = args[1]
outdir = args[2]

process_all_author_gender(nature_dir, cited_dois_dir, outdir)

