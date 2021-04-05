
require(jsonlite)
require(data.table)
require(here)
require(stringr)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))
source(file.path(proj_dir, "/analysis_scripts/analysis_utils.R"))





#' Read all nature json files and get author info
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
#' @return dataframe, all_authors author infos for nature articles
read_nature_country_json_files <- function(nature_dir){

    json_res_files = list.files(nature_dir, pattern=".json", full.names = TRUE)
    
    all_authors = NA
    for(curr_file in json_res_files){

        print(curr_file)
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        # format authors
        # get the affiliation for each author, and put the country first
        # affiliation info is assumed to be split by commas, with country
        # as the last element
        country_affil = lapply(json_res$authors, function(x) lapply(str_split(unlist(x$affiliation), ", "), function(x) rev(x)[[1]]))
        country_affil = unlist(lapply(country_affil, function(x) paste(unique(unlist(x)), collapse="; ")))

        # make df
        authors_df = data.frame(file_id=json_res$file_id,
                                year=json_res$year,
                                country_affil=country_affil)
        
        all_authors = rbind(all_authors, authors_df)

    }

    all_authors = all_authors[-1,]

    # format file_id into a doi
    all_authors$doi = paste("doi:10.1038/", all_authors$file_id, sep="")

    # split the countries into multiple rows
    all_authors = separate_rows(all_authors, country_affil, sep="; ")

    # format the country name
    all_authors$country_affil = format_country_names(all_authors$country_affil)

    all_authors = data.frame(all_authors)

    return(all_authors)

}

get_nature_news_mentions <- function(){


    # read in all location tables from nature news
    full_loc_df = NA
    loc_files = list.files(paste(proj_dir,"/data/scraped_data/", sep=""), full.names = T)
    loc_files = grep("location_table_raw_", loc_files, value=T)
    for(loc_file in loc_files){

        loc_df = read_corenlp_location_files(loc_file)
        loc_df$year = str_extract(loc_file, "[1-9][0-9]+") # curr_year

        full_loc_df = rbind(full_loc_df, loc_df)
    }
    full_loc_df = full_loc_df[-1,]

    full_loc_df = subset(full_loc_df, est_un_region != "" & 
                                            est_un_subregion != "" &
                                            est_un_region != "NO_EST" & 
                                            est_un_subregion != "NO_EST")

    full_loc_df = unique(full_loc_df[,c("est_country_code", "file_id", "year")])
    colnames(full_loc_df) = c("address.country_code", "file_id", "year")
    full_loc_df$corpus = "naturenews_mentions"

    return(full_loc_df)

}

get_nature_bg <- function(nature_dir){

    # all the nature articles
    nature_country_df = read_nature_country_json_files(nature_dir)
    colnames(nature_country_df)[3] = "country"
    nature_country_df = subset(nature_country_df, country != "")

    # now process them to format countries
    nature_country_df_formatted = get_author_country(nature_country_df)
    nature_country_df_formatted = unique(na.omit(nature_country_df_formatted))
    nature_country_df_formatted = subset(nature_country_df_formatted,  address.country_code != "NONE")

    nature_country_df_formatted$corpus = "nature_articles"

    nature_country_df_formatted = unique(nature_country_df_formatted[,c("address.country_code",
                                                                         "file_id", "year", "corpus")])


    return(nature_country_df_formatted)

}

get_nature_news_cited <- function(ref_dir){

    # all the cited articles
    cited_country_file = file.path(proj_dir, 
                                    "/data/author_data/cited_author_country.tsv")
    cited_country_df = data.frame(fread(cited_country_file))
    cited_country_df = subset(cited_country_df, country != "")
    cited_country_df$country = format_country_names(cited_country_df$country)

    # get the year from the gender file
    cited_gender_file = file.path(proj_dir, 
                                    "/data/author_data/cited_author_gender.tsv")
    cited_gender_df = data.frame(fread(cited_gender_file))
    cited_gender_df = unique(cited_gender_df[,c("file_id", "year")])
    cited_country_df = merge(cited_country_df, cited_gender_df)

    # format the countries
    cited_country_df_formatted = get_author_country(cited_country_df)
    cited_country_df_formatted = unique(cited_country_df_formatted)

    # we only care about if a country was cited in an article, 
    # not how many times it was cited
    cited_country_df_formatted$num_entries = 1

    cited_country_df_formatted$corpus = "naturenews_citations"

    cited_country_df_formatted = unique(cited_country_df_formatted[,c("address.country_code",
                                                                         "file_id", "year", "corpus")])



    return(cited_country_df_formatted)

}

get_springer_bg <- function(){

    # all the springer articles
    springer_country_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_random_country_cache.tsv")
    springer_country_df = data.frame(fread(springer_country_file))
    springer_country_df = subset(springer_country_df, country != "" )
    springer_country_df = springer_country_df[,c("country", "doi", "year", "num_entries")]


    # format the countries
    springer_country_df$country = format_country_names(springer_country_df$country)
    springer_country_df_formatted = get_author_country(springer_country_df)
    springer_country_df_formatted = unique(springer_country_df_formatted)
    springer_country_df_formatted$num_entries[
        is.na(springer_country_df_formatted$num_entries)] = 0


    # format the result
    colnames(springer_country_df_formatted)[2] = "file_id"
    springer_country_df_formatted$corpus = "springer_articles"

    springer_country_df_formatted = unique(springer_country_df_formatted[,c("address.country_code",
                                                                         "file_id", "year", "corpus")])

    return(springer_country_df_formatted)


}

#' Compute country bootstrap CI
#' this works by taking a random subset of articles per year
#' and calculating the bootstrap mean, upperCI and lowerCI
#' its assumed that there exists a column called country
#'
#' @param sum_country This is the full data to be explored
#' @return a dataframe of the CI estimates
get_bootstrapped_CI_country_corpus <- function(sum_country){

    # get the full country info
    un_info = get_country_info()
    country_df = merge(sum_country, un_info)

    # for each country, in each corpus calculate the bootstrap CI
    bootstrap_country_df = NA
    for(curr_country in unique(country_df$country)){
        print(curr_country)
        for(curr_corpus in c("naturenews_mentions", "naturenews_citations")){
            print(curr_corpus)

            in_df = data.frame(subset(country_df, corpus==curr_corpus))
            bootstrap_res = compute_bootstrap_location(
                                in_df, 
                                year_col_id = "year", 
                                article_col_id = "file_id", 
                                country_col_id = "country",
                                country_agg = curr_country, 
                                conf_int = 0.95)
            bootstrap_res$country = curr_country
            bootstrap_res$corpus = curr_corpus
            bootstrap_country_df = rbind(bootstrap_country_df, bootstrap_res)
        }
    }
    bootstrap_country_df = bootstrap_country_df[-1,]
    return(bootstrap_country_df)
}


#' Read all json files to do the gender prediction
#' the nature background authors
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
process_all_author_country <- function(nature_dir, cited_dois_dir, outdir){

    # we have 3 source files and 1 sourcwe folder that needs to be processed for author country info

    # background files are NAture and springer
    springer_country_df_formatted = get_springer_bg()
    

    nature_country_df_formatted = get_nature_bg(nature_dir)
   
    # foreground files are Nature mentions and citations
    cited_country_df_formatted = get_nature_news_cited(cited_dois_dir)
    
    mention_country_df_formatted = get_nature_news_mentions()
    
    # now put it all together
    # put springer back in later springer_country_df_formatted
    sum_country = rbind(nature_country_df_formatted[,colnames(mention_country_df_formatted)],
                        springer_country_df_formatted[,colnames(mention_country_df_formatted)],
                        cited_country_df_formatted[,colnames(mention_country_df_formatted)],
                        mention_country_df_formatted)

    author_country_file = file.path(outdir, "all_author_country.tsv")
    write.table(sum_country, file=author_country_file, sep="\t", quote=F, row.names=F)

    bootstrap_country_df = get_bootstrapped_CI_country_corpus(sum_country)
    bootstrap_country_file = file.path(outdir, "all_author_country_95CI.tsv")
    write.table(bootstrap_country_df, file=bootstrap_country_file, sep="\t", quote=F, row.names=F)


}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
nature_dir = args[1]
cited_dois_dir = args[2]
outdir = args[3]

process_all_author_country(nature_dir, cited_dois_dir, outdir)
