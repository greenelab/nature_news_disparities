require(jsonlite)
require(data.table)
require(dplyr)
require(here)


proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))

BOOTSTRAP_SIZE=5000

# read in all word frequencies for faster compute
FREQ_FILE = file.path(proj_dir, "data/scraped_data/freq_table_raw.tsv")
if(!exists("FREQ_DF")){
    FREQ_DF = data.frame(fread(FREQ_FILE))
}


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

#' public method that reads in the name origine prediction 
#' files and formats it into a dataframe
#' 
#' @param name_pred_file, prediction results from Wiki2019-LSTM
#' @param name_info_file, file used as input to Wiki2019-LSTM
#' @return dataframe of name origin predictions with annotation info
read_name_origin <- function(name_pred_file, name_info_file){
        
    # read in the name data
    name_pred_df = data.frame(fread(name_pred_file))
    name_info_df = data.frame(fread(name_info_file))
    
    # format the prediction table
    colnames(name_pred_df)[1] = "author"
    name_origin_vec = colnames(name_pred_df)[2:ncol(name_pred_df)]
    name_origin = apply(name_pred_df[,2:ncol(name_pred_df)], 1, 
                        function(x) name_origin_vec[which(x == max(x))])
    name_pred_df$name_origin = name_origin
    
    name_df = merge(name_info_df, name_pred_df[,c("author", "name_origin")])
    
    # remove any names that may have got through that are not real names
    name_df = name_df[grep("collab|group", tolower(name_df$author), invert=T), ]
    name_df = unique(name_df)
    
    
    return(name_df)
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

    # make sure there is an author_pos column
    stopifnot("author_pos" %in% colnames(full_data_df))

    # and the column has the right values
    author_pos_val = unique(full_data_df$author_pos)
    author_pos_val = union(author_pos_val, c("first", "last"))
    print(length(author_pos_val))
    stopifnot(length(author_pos_val) == 2)

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
compute_bootstrap_gender <- function(full_data_df, year_col_id, article_col_id, conf_int, gender_search_str='MALE'){

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

        year_df = aggregate(year_df$est_gender, list(year_df$art_id), function(x) c(sum(x==gender_search_str), length(x)))
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

        boot_res = rep(NA, BOOTSTRAP_SIZE)
        if(length(table(year_df$is_country_present)) != 2){
            # if there was no observation for this year, skip
            boot_res = rep(0, BOOTSTRAP_SIZE)
        }else{
            # get article id's to sample
            for(idx in 1:BOOTSTRAP_SIZE){

                boot_samp = sample_n(year_df, nrow(year_df), replace=T)
                percent_country = sum(boot_samp$is_country_present, na.rm=T) / 
                                nrow(year_df)
                boot_res[idx] = percent_country
                
            }
        }

        quantile_res[quantile_res$year == curr_year,] = 
            data.frame(curr_year, 
                        quantile(boot_res, 1-conf_int),
                        quantile(boot_res, conf_int),
                        mean(boot_res))

    }

    return(quantile_res)

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


#' Get the word frequencies across multiple JSON files containing
#' for all years and news types
#'
#' @param class_ids This contains at a minimum a file_id - country - 
#' article_type - year mapping
#' @param class_str name of country class of interest
#' @return word_freq a dataframe of word counts across all relevant articles
get_word_freq_per_class <- function(class_ids, class_str, run_bootstrap=FALSE){
    
    # calculate the word frequency
    curr_file_ids = unique(class_ids$file_id)
    if(run_bootstrap){
        curr_file_ids = sample(curr_file_ids, length(curr_file_ids), replace=T)
    }
    all_articles_df = subset(FREQ_DF, file_id %in% curr_file_ids)

    # sum word frequencies across the different JSON files
    class_word_freq = all_articles_df %>%
                    select("word", "count") %>%
                    group_by(word) %>% 
                    summarise(sum(count)) 
    col_id = paste(class_str, "_count", sep="")
    colnames(class_word_freq)[2] = col_id
    
    # sort
    class_word_freq = class_word_freq[
                            order(class_word_freq[,col_id], 
                                  decreasing=T),]
     
    return(class_word_freq)
}




#' Get the word frequencies across multiple JSON files containing
#' for all years and news types. This additionally processes the counts
#' and normalizes the frequency of the word by all observed articles
#'
#' @param class_ids This contains at a minimum a file_id - country - 
#' article_type - year mapping
#' @param class_str name of country class of interest
#' @param class_all_word_freq output from get_word_freq_per_class using all articles
#' @param min_freq, the minimum frequency of a word in class_all_word_freq to be considered 
#' @return all_country_word_freq list of dataframes, each element is a dataframe 
#' of word counts across all relevant articles from a country
get_word_freq_per_country <- function(mentions_df, class_str, 
                                          class_all_word_freq, min_freq){

    count_str = paste0(class_str, "_count")
    
    all_country_word_freq = list()
    for(curr_country in unique(mentions_df$address.country_code)){
        
        # get the word freq for class C mentions
        class_word_freq = get_word_freq_per_class(
                                subset(mentions_df, address.country_code == curr_country), 
                                class_str = class_str)
        
        # merge with the word freq for entire corpus
        per_class_word_freq = merge(data.table(class_word_freq), 
                                    data.table(class_all_word_freq), by="word")
        
            
        # word should be used at least 100 time in the full corpus
        per_class_word_freq = subset(per_class_word_freq, class_all_count > min_freq)
            
        # get the word frequency scaled by corpus frequency
        per_class_word_freq$ratio = subset(per_class_word_freq, select=c(count_str)) / 
                                    per_class_word_freq$class_all_count
        
        # get top words per country
        per_class_word_freq = per_class_word_freq[order(per_class_word_freq$ratio, decreasing=T),]
        
        # save top words
        per_class_word_freq_df = subset(per_class_word_freq, select=c("word", count_str))
        colnames(per_class_word_freq_df)[2] = paste("count", curr_country, sep="_")
        all_country_word_freq[[curr_country]] = per_class_word_freq_df

    }

    return(all_country_word_freq)
}


#' Get the word frequencies across multiple JSON files containing
#' for all years and news types. This additionally processes the counts
#' and normalizes the frequency of the word by all observed articles
#'
#' @param class_c_mentions This contains at a minimum a file_id - country - 
#' article_type - year mapping for all class c countries
#' @param class_c_mentions This contains at a minimum a file_id - country - 
#' article_type - year mapping for all class m countries
#' @param word_vec the word of interest to calculate the bootstrap over
#' @return boot_res data frame of the bootstrap stats for each word
get_bootstrap_word_ratio <- function(class_c_mentions, class_m_mentions, word_vec, conf_int=0.95){

    set.seed(5)

    # run bootstrap samples
    boot_res = data.frame(matrix(ncol = BOOTSTRAP_SIZE, nrow = length(word_vec)))
    for(idx in 1:BOOTSTRAP_SIZE){
    
        # first calculate word counts for M countries
        all_country_word_freq_m = list()
        for(curr_country in unique(class_m_mentions$address.country_code)){
            
            # get the word freq for class M mentions
            class_word_freq = get_word_freq_per_class(
                                    subset(class_m_mentions, address.country_code == curr_country), 
                                    class_str = "class_m",
                                    run_bootstrap = TRUE)
            class_word_freq = subset(class_word_freq, word %in% word_vec)
            
            all_country_word_freq_m[[curr_country]] = class_word_freq
    
        }
        # now that we have it for each country, take the median across all countries
        mentions_m_freq = Reduce(function(x, y) merge(x, y, by = "word", all = T), 
                           all_country_word_freq_m)
        mentions_m_freq[is.na(mentions_m_freq)] = 0
        mentions_m_freq$median_m = apply(mentions_m_freq[,2:ncol(mentions_m_freq)], 
                                           1, median)
        
        # first calculate word counts for C countries
        all_country_word_freq_c = list()
        for(curr_country in unique(class_c_mentions$address.country_code)){
            
            # get the word freq for class C mentions
            class_word_freq = get_word_freq_per_class(
                                    subset(class_c_mentions, address.country_code == curr_country), 
                                    class_str = "class_c",
                                    run_bootstrap = TRUE)
            class_word_freq = subset(class_word_freq, word %in% word_vec)
            
            all_country_word_freq_c[[curr_country]] = class_word_freq
    
        }
        # now that we have it for each country, take the median across all countries
        mentions_c_freq = Reduce(function(x, y) merge(x, y, by = "word", all = T), 
                           all_country_word_freq_c)
        mentions_c_freq[is.na(mentions_c_freq)] = 0
        mentions_c_freq$median_c = apply(mentions_c_freq[,2:ncol(mentions_c_freq)], 
                                           1, median)
        
        # now lets compare class C and M countries by taking the ratio
        mentions_joint = merge(mentions_m_freq[,c("word", "median_m")],
                               mentions_c_freq[,c("word", "median_c")])
        mentions_joint$ratio = (mentions_joint$median_c+1) / (mentions_joint$median_m+1)

        # format table
        rownames(mentions_joint) = mentions_joint$word
        mentions_joint = mentions_joint[word_vec,]
        boot_res[,idx] = mentions_joint$ratio
        
        print(idx)
    }

    # now take the quantiles to return bootstrap estimates
    rownames(boot_res) = word_vec
    quantile_res = data.frame(rownames(boot_res), 
                        apply(boot_res, 1, quantile, 1-conf_int, na.rm=TRUE),
                        apply(boot_res, 1, quantile, conf_int, na.rm=TRUE),
                        apply(boot_res, 1, mean, na.rm=TRUE))

    colnames(quantile_res) = c("word", "bottom_CI", "top_ci", "mean")


    return(list(quantile_res, boot_res))
}

