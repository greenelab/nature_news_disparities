
require(jsonlite)
require(data.table)
require(here)
require(stringr)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))
source(file.path(proj_dir, "/analysis_scripts/analysis_utils.R"))


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

    # get the num of mentions per article
    full_loc_df_formatted = full_loc_df %>% 
                                group_by(year, est_country_code) %>% 
                                summarise(n()) 
    full_loc_df_formatted = data.frame(full_loc_df_formatted)
    colnames(full_loc_df_formatted) = c("year", "address.country_code", "num_entries")
    full_loc_df_formatted$corpus = "nature_news"


    # store the number of articles per year for later
    num_files_nature_news = unique(full_loc_df[,c("year", "file_id")]) %>% 
                        group_by(year) %>% 
                        summarise(n()) 

    num_files_nature_news = data.frame(num_files_nature_news)
    colnames(num_files_nature_news)[2] = "tot_articles"
    num_files_nature_news$corpus = "nature_news"

    return(list(full_loc_df_formatted, num_files_nature_news))

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


    # now sum over the articles
    nature_country_df_counts = nature_country_df_formatted %>% 
                                group_by(year, address.country_code) %>% 
                                summarise(n()) 
    nature_country_df_counts = data.frame(nature_country_df_counts)
    colnames(nature_country_df_counts) = c("year", "address.country_code", "num_entries")
    nature_country_df_counts$corpus = "nature_articles"


    nature_article_df_counts = unique(nature_country_df_formatted[,c("year", "doi")]) %>% 
                                group_by(year) %>% 
                                summarise(n()) 
    nature_article_df_counts = data.frame(nature_article_df_counts)
    colnames(nature_article_df_counts)[2] = "tot_articles"
    nature_article_df_counts$corpus = "nature_articles"

    return(list(nature_country_df_counts, nature_article_df_counts))

}

get_nature_news_cited <- function(){

    # all the cited articles
    cited_country_file = file.path(proj_dir, 
                                    "/data/author_data/cited_author_country.tsv")
    cited_country_df = data.frame(fread(cited_country_file))
    colnames(cited_country_df)[3] = "num_entries"
    cited_country_df = subset(cited_country_df, country != "")
    cited_country_df$country = format_country_names(cited_country_df$country)

    # format the countries
    cited_country_df_formatted = get_author_country(cited_country_df)
    cited_country_df_formatted = unique(cited_country_df_formatted)

    # count citations
    cited_country_df_formatted = aggregate(cited_country_df_formatted$num_entries, 
                            by=cited_country_df_formatted[,c("year", "address.country_code")],
                           FUN=sum)                     
    colnames(cited_country_df_formatted)[3] = "num_entries"

    cited_country_df_formatted$corpus = "news_citation"


    # get the number of total number of cited articles
    # get the doi info
    ref_dois_df = get_ref_dois(ref_dir)

    # already cached springer dataframe
    cache_file = file.path(ref_data_dir, "/springer_cited_author_cache.tsv")
    cache_df = data.frame(fread(cache_file))
    cache_df = unique(na.omit(cache_df[,c("doi", "publisher")]))
    dois_found_df = merge(ref_dois_df, cache_df)
    dois_found_df = unique(dois_found_df)
    num_files_cited = unique(dois_found_df[,c("year", "doi")]) %>% 
                        group_by(year) %>% 
                        summarise(n()) 
    num_files_cited = data.frame(num_files_cited)
    colnames(num_files_cited)[2] = "tot_articles"
    num_files_cited$corpus = "news_citation"

    return(list(cited_country_df_formatted, num_files_cited))

}

get_springer_bg <- function(){

    # all the springer articles
    springer_country_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_country_cache.tsv")
    springer_country_df = data.frame(fread(springer_country_file))
    springer_country_df = springer_country_df[,1:3]
    springer_country_df = subset(springer_country_df, country != "")


    # format the countries
    springer_country_df$country = format_country_names(springer_country_df$country)
    springer_country_df_formatted = get_author_country(springer_country_df)
    springer_country_df_formatted = unique(springer_country_df_formatted)
    springer_country_df_formatted$num_entries[
        is.na(springer_country_df_formatted$num_entries)] = 0

    # count citations
    springer_country_df_formatted = aggregate(springer_country_df_formatted$num_entries, 
                            by=springer_country_df_formatted[,c("year", "address.country_code")],
                            FUN=sum)                    
    colnames(springer_country_df_formatted)[3] = "num_entries"

    springer_country_df_formatted$corpus = "springer"




    # get num springr articles
    springer_year_file = file.path(ref_data_dir, "/springer_year_cache.tsv")
    num_files_springer = data.frame(fread(springer_year_file))
    num_files_springer = num_files_springer[,1:2]
    colnames(num_files_springer)[2] = "tot_articles"
    num_files_springer$corpus = "springer"

    return(list(springer_country_df_formatted, num_files_springer))


}


#' Read all json files to do the gender prediction
#' the nature background authors
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
process_all_author_country <- function(nature_dir, outdir){

    # we have 3 source files and 1 sourcwe folder that needs to be processed for author country info

    # background files are NAture and springer
    springer_res = get_springer_bg()
    springer_country_df_formatted = springer_res[[1]]
    num_files_springer = springer_res[[2]]

    nature_bg_res = get_nature_bg(nature_dir)
    nature_country_df_formatted = nature_bg_res[[1]]
    num_files_nature = nature_bg_res[[2]]

    # foreground files are Nature mentions and citations
    nn_cited_res = get_nature_news_cited()
    cited_country_df_formatted = nn_cited_res[[1]]
    num_files_cited = nn_cited_res[[2]]

    nn_mention_res = get_nature_news_mentions()
    mention_country_df_formatted = nn_mention_res[[1]]
    num_files_mention = nn_mention_res[[2]]

    # now put it all together
    sum_country = rbind(springer_country_df_formatted, nature_country_df_formatted,
                        cited_country_df_formatted, mention_country_df_formatted)

    author_country_file = file.path(outdir, "all_author_country.tsv")
    write.table(sum_country, file=author_country_file, sep="\t", quote=F, row.names=F)

    
    num_articles = rbind(num_files_springer, num_files_nature, 
                        num_files_cited, num_files_mention)
    author_year_file = file.path(outdir, "total_num_articles_per_corpus.tsv")
    write.table(num_articles, file=author_year_file, sep="\t", quote=F, row.names=F)



}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
nature_dir = args[1]
outdir = args[2]

process_all_author_country(nature_dir, outdir)

