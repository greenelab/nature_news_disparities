
require(jsonlite)
require(data.table)
require(here)
require(stringr)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scrape_utils.R"))


format_country_names <- function(country_vec){

    country_vec = lapply(country_vec, 
                        function(x) gsub("[[:punct:]]", "", x))

    country_vec = lapply(country_vec, 
                        function(x) gsub("[[:digit:]]+", "", x))

    country_vec = unlist(country_vec)
    country_vec = str_trim(country_vec)
    country_vec = tolower(country_vec)


    return(country_vec)
}


get_author_country <- function(loc_df){

    # now query open street map to get the country codes
    osm_res = batch_osm_query(unique(loc_df$country))
    colnames(osm_res)[which(colnames(osm_res) == "query")] = "country"
    loc_df = merge(loc_df, 
                        osm_res[,c("country", "address.country_code")],
                        all=T)
    loc_df$address.country_code[
        which(is.na(loc_df$address.country_code))] = "NOT_FOUND"

    locs_df = unique(loc_df)


    return(loc_df)

}



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

    # now turn it into counts by year
    sum_country = all_authors %>% 
                    group_by(year, country_affil) %>%
                    summarize(n())

    sum_country = data.frame(sum_country)
    colnames(sum_country) = c("year", "country", "num_entries")
    return(sum_country)

}


#' Read all json files to do the gender prediction
#' the nature background authors
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
process_all_author_country <- function(nature_dir, outdir){

    # we have 1 source file that needs to be processed for author country info
    
    # then all the nature articles
    nature_country_df = read_nature_country_json_files(nature_dir)

    # now process them to format countries
    nature_country_df_formatted = get_author_country(nature_country_df)

    # sum again over countries because multiple id's can map to the same country
    sum_country = aggregate(nature_country_df_formatted$num_entries, 
                            by=nature_country_df_formatted[,c("year", "address.country_code")],
                            FUN=sum)

    nature_author_country_file = file.path(outdir, "nature_author_country.tsv")
    write.table(sum_country, file=nature_author_country_file, sep="\t", quote=F, row.names=F)




}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
nature_dir = args[1]
cited_dois_dir = args[1]
outdir = args[2]

process_all_author_gender(nature_dir, cited_dois_dir, outdir)

