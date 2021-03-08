library(jsonlite)
library(data.table)
library(dplyr)


proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))


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

