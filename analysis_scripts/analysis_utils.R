library(jsonlite)
library(data.table)
library(dplyr)
library(here)


proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))

BOOTSTRAP_SIZE=10

#' Read in the quote information from processed coreNLP TSV output
#'
#' @param corenlp_file The processed coreNLP output TSV file.
#' Expected column names are full_name, gender, and quote
#' @return a dataframe of the quote information
read_corenlp_quote_files <- function(corenlp_file){
    
    corenlp_df = data.frame(fread(corenlp_file, header=T, quote=""))
    colnames(corenlp_df)[which(colnames(corenlp_df)=="full_name")] = "est_speaker"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="gender")] = "est_gender"

    corenlp_df$est_speaker[which(is.na(corenlp_df$est_speaker))] = "NO_EST"
    corenlp_df$est_gender[which(is.na(corenlp_df$est_gender))] = "NO_EST"

    corenlp_df$quote = gsub("\"", "", corenlp_df$quote)


    return(corenlp_df)
}

#' Read in the benchmark quote information from processed coreNLP TSV output
#'
#' @param gold_file The hand-processed coreNLP output TSV file.
#' Expected column names are full_name, gender, quote
#' @return a dataframe of the benchmark quote information
read_benchmark_quote_file <- function(gold_file){
    
    gold_df = data.frame(fread(gold_file, header=T, quote=FALSE))
    colnames(gold_df)[which(colnames(gold_df)=="full_name")] = "true_speaker"
    colnames(gold_df)[which(colnames(gold_df)=="gender")] = "true_gender"

    gold_df$quote = gsub("\"", "", gold_df$quote)
    return(gold_df)

}

#' Read in the location information from processed coreNLP TSV output
#'
#' @param corenlp_file The processed coreNLP output TSV file.
#' Expected column names are address.country_code, country, un_region, and un_subregion
#' @return a dataframe of the location information
read_corenlp_location_files <- function(corenlp_file){
    
    corenlp_df = data.frame(fread(corenlp_file, header=T))
    country_df = get_country_info()
    corenlp_df = merge(corenlp_df, country_df, all.x=T)
    corenlp_df = unique(corenlp_df)

    colnames(corenlp_df)[which(colnames(corenlp_df)=="address.country_code")] = "est_country_code"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="country")] = "est_country"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="un_region")] = "est_un_region"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="un_subregion")] = "est_un_subregion"

    corenlp_df$est_country[which(is.na(corenlp_df$est_country_code))] = "NO_EST"
    corenlp_df$est_country[which(is.na(corenlp_df$est_country))] = "NO_EST"
    corenlp_df$est_un_region[which(is.na(corenlp_df$est_un_region))] = "NO_EST"
    corenlp_df$est_un_subregion[which(is.na(corenlp_df$est_un_subregion))] = "NO_EST"


    return(corenlp_df)
}

#' Read in the benchmark location information from processed coreNLP TSV output
#'
#' @param bm_loc_file The  hand-processed coreNLP output TSV file.
#' Expected column names are address.country_code, country, un_region, and un_subregion
#' @return a dataframe of the benchmark location information
read_benchmark_location_file <- function(bm_loc_file){
    
    gold_df = data.frame(fread(bm_loc_file, header=T))
    colnames(gold_df)[which(colnames(gold_df)=="address.country_code")] = "true_country_code"
    colnames(gold_df)[which(colnames(gold_df)=="country")] = "true_country"
    colnames(gold_df)[which(colnames(gold_df)=="un_region")] = "true_un_region"
    colnames(gold_df)[which(colnames(gold_df)=="un_subregion")] = "true_un_subregion"
    return(gold_df)

}



#' Read in the gender-prediction from citation or background files
#'
#' @param full_data_df This is the full data to be explored
#' @param conf_int Numeric, this is the range the Ci is calculated
#' @return a dataframe of the CI estimates
read_gender_files <- function(in_file){
    in_df = data.frame(fread(in_file))
    colnames(in_df)[which(colnames(in_df) == "guessed_gender")] = "est_gender"
    in_df = subset(in_df, !is.na(year))
    return(in_df)
}


#' Compute first v last bootstrap CI
#' this works by taking a random subset of articles per year
#' and calculating the bootstrap mean, upperCI and lowerCI
#' its assumed that there exists a column called author_pos
#' where there are only two values -- first / last
#'
#' @param full_data_df This is the full data to be explored
#' @param year_col_id This is column name containing year to be selected by
#' @param article_col_id This is column name containing article ids to be selected by
#' @param conf_int between 0-1, this is the range the CI is calculated
#' @return a dataframe of the CI estimates
compute_bootstrap_first_author <- function(full_data_df, year_col_id, article_col_id, conf_int){

    set.seed(5)

    in_df = data.frame(year = full_data_df[,year_col_id],
                        art_id = full_data_df[,article_col_id],
                        author_pos = full_data_df$author_pos)
    
    # we need to get a bootstrap sample for each year
    quantile_res = data.frame(year=unique(in_df$year),
                                bottom_CI = NA,
                                top_CI = NA,
                                mean = NA)
    for(curr_year in unique(in_df$year)){
        year_df = subset(in_df, year == curr_year)

        year_df = aggregate(year_df$author_pos, list(year_df$art_id), function(x) c(sum(x=='first'), length(x)))
        year_df = data.frame(as.matrix(year_df))
        colnames(year_df) = c("art_id", "num_first", "num_total")
        year_df$num_first = as.numeric(year_df$num_first)
        year_df$num_total = as.numeric(year_df$num_total)

        # get article id's to sample
        curr_ids = unique(year_df$art_id)
        bootstrap_size = length(curr_ids)
        boot_res = rep(NA, BOOTSTRAP_SIZE)
        for(idx in 1:BOOTSTRAP_SIZE){

            boot_samp = sample_n(year_df, nrow(year_df), replace=T)
            percent_first = sum(boot_samp$num_first, na.rm=T) / 
                            sum(boot_samp$num_total, na.rm=T)
            boot_res[idx] = percent_first
            
        }

        quantile_res[quantile_res$year == curr_year,] = 
            data.frame(curr_year, 
                        quantile(boot_res, 1-conf_int),
                        quantile(boot_res, conf_int),
                        mean(boot_res))

    }

    return(quantile_res)

}


#' Compute bootstrap CI
#' this works by taking a random subset of articles per year
#' and calculating the bootstrap mean, upperCI and lowerCI
#' its assumed that there exists a column called est_gender
#'
#' @param full_data_df This is the full data to be explored
#' @param year_col_id This is column name containing year to be selected by
#' @param article_col_id This is column name containing article ids to be selected by
#' @param conf_int between 0-1, this is the range the CI is calculated
#' @return a dataframe of the CI estimates
compute_bootstrap_gender <- function(full_data_df, year_col_id, article_col_id, conf_int){

    set.seed(5)

    in_df = data.frame(year = full_data_df[,year_col_id],
                        art_id = full_data_df[,article_col_id],
                        est_gender = full_data_df$est_gender)
    
    # we need to get a bootstrap sample for each year
    quantile_res = data.frame(year=unique(in_df$year),
                                bottom_CI = NA,
                                top_CI = NA,
                                mean = NA)
    for(curr_year in unique(in_df$year)){
        year_df = subset(in_df, year == curr_year)

        year_df = aggregate(year_df$est_gender, list(year_df$art_id), function(x) c(sum(x=='MALE'), length(x)))
        year_df = data.frame(as.matrix(year_df))
        colnames(year_df) = c("art_id", "num_male", "num_total")
        year_df$num_male = as.numeric(year_df$num_male)
        year_df$num_total = as.numeric(year_df$num_total)

        # get article id's to sample
        curr_ids = unique(year_df$art_id)
        bootstrap_size = length(curr_ids)
        boot_res = rep(NA, BOOTSTRAP_SIZE)
        for(idx in 1:BOOTSTRAP_SIZE){

            boot_samp = sample_n(year_df, nrow(year_df), replace=T)
            percent_male = sum(boot_samp$num_male, na.rm=T) / 
                            sum(boot_samp$num_total, na.rm=T)
            boot_res[idx] = percent_male
            
        }

        quantile_res[quantile_res$year == curr_year,] = 
            data.frame(curr_year, 
                        quantile(boot_res, 1-conf_int),
                        quantile(boot_res, conf_int),
                        mean(boot_res))

    }

    return(quantile_res)

}

#' Compute location bootstrap CI
#' this works by taking a random subset of articles per year
#' and calculating the bootstrap mean, upperCI and lowerCI
#' its assumed that there exists a column called country
#'
#' @param full_data_df This is the full data to be explored
#' @param year_col_id This is column name containing year to be selected by
#' @param article_col_id This is column name containing article ids to be selected by
#' @param conf_int between 0-1, this is the range the CI is calculated
#' @return a dataframe of the CI estimates
compute_bootstrap_location <- function(full_data_df, year_col_id, article_col_id, country_col_id, country_agg, conf_int){

    set.seed(5)

    in_df = data.frame(year = full_data_df[,year_col_id],
                        art_id = full_data_df[,article_col_id],
                        est_loc = full_data_df[,country_col_id])
    
    # we need to get a bootstrap sample for each year
    quantile_res = data.frame(year=unique(in_df$year),
                                bottom_CI = NA,
                                top_CI = NA,
                                mean = NA)
    for(curr_year in unique(in_df$year)){
        year_df = subset(in_df, year == curr_year)

        year_df = aggregate(year_df$est_loc, list(year_df$art_id), function(x) any(x==country_agg))
        year_df = data.frame(as.matrix(year_df))
        colnames(year_df) = c("art_id", "is_country_present")
        year_df$is_country_present[year_df$is_country_present == "TRUE"] = 1
        year_df$is_country_present[year_df$is_country_present == "FALSE"] = 0
        year_df$is_country_present = as.numeric(year_df$is_country_present)

        # get article id's to sample
        boot_res = rep(NA, BOOTSTRAP_SIZE)
        for(idx in 1:BOOTSTRAP_SIZE){

            boot_samp = sample_n(year_df, nrow(year_df), replace=T)
            percent_country = sum(boot_samp$is_country_present, na.rm=T) / 
                            nrow(year_df)
            boot_res[idx] = percent_country
            
        }

        quantile_res[quantile_res$year == curr_year,] = 
            data.frame(curr_year, 
                        quantile(boot_res, 1-conf_int),
                        quantile(boot_res, conf_int),
                        mean(boot_res))

    }

    return(quantile_res)

}


#' Get the word frequencies from a JSON file containing
#' file_ids and the body of an articles
#'
#' @param json_file This contains all the text from a specific year and news-type
#' @param file_ids_pass optional parameter to only include specific file_ids
#' @return word_freq a dataframe of word counts
calc_word_freq <- function(json_file, file_ids_pass=NA){
    
        # read in the json
        all_articles_df = read_json(json_file)
        
        # get the articles we are interested in
        if(!is.na(file_ids_pass)){
            all_articles_df = subset(all_articles_df, file_id %in% file_ids_pass)
        }
        all_articles_df = data.table(all_articles_df)
        
        # tokenize
        all_articles_df <- all_articles_df %>%
                unnest_tokens(word, body)
        
        # remove stop words
        all_articles_df <- all_articles_df %>%
                anti_join(stop_words)
        
        # make sure its ASCII
        all_articles_df$word = iconv(all_articles_df$word, from = "UTF-8", to = "ASCII", sub = "")
        
        # remove punctuations
        all_articles_df$word = gsub('[[:punct:] ]+', '', all_articles_df$word)
        
        # make sure its not a number
        non_num = which(is.na(as.numeric(all_articles_df$word)))
        all_articles_df = all_articles_df[non_num,]
        
        # make sure its not an empty string
        non_whitespace = which(trimws(all_articles_df$word)!="")
        all_articles_df = all_articles_df[non_whitespace,]

        # get counts
        word_freq = all_articles_df %>%
                count(word, sort = TRUE)
        
        return(word_freq)
}

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
