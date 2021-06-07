require(jsonlite)
require(data.table)
require(dplyr)
require(tidytext)
require(textclean)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/analysis_scripts/analysis_utils.R"))

YEARS=2005:2020
TARGET_TYPES=c("news", "news-and-views", "news-feature", "toolbox", 
                "technology-feature", "career-column", "career-feature")


#' Get the word frequencies from a JSON file containing
#' file_ids and the body of an articles
#'
#' @param json_file This contains all the text from a specific year and news-type
#' @param file_ids_pass optional parameter to only include specific file_ids
#' @return word_freq a dataframe of word counts
calc_word_freq <- function(all_articles_df){

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



#' Get the word frequencies across multiple JSON files containing
#' for all years and news types
#'
#' @param class_ids This contains at a minimum a file_id - country - 
#' article_type - year mapping
#' @param class_str name of country class of interest
#' @return word_freq a dataframe of word counts across all relevant articles
make_word_freq_table <- function(){
        
    # get the word frequencies for class C articles
    all_word_freq = NA
    for(curr_year in YEARS){
        for(curr_type in TARGET_TYPES){

            print(c(curr_year, curr_type))

            # make the file name
            curr_file = file.path(proj_dir, "/data/scraped_data/downloads/",
                                  paste("links_crawled_", 
                                        curr_year, "_", 
                                        curr_type, ".json", sep=""))
            
            # read in the json
            file_size = file.info(curr_file)$size
            if(file_size == 0){
                next
            }
            curr_articles_df = read_json(curr_file)

            # strip any remaining html
            curr_articles_df$body = replace_html(curr_articles_df$body)

            # get token freq
            class_word_freq = curr_articles_df %>%
                                group_by(file_id) %>% 
                                calc_word_freq


            class_word_freq = as.data.frame(class_word_freq)
            colnames(class_word_freq)[3] = "count"

            # add info
            class_word_freq$year = curr_year
            class_word_freq$type = curr_type


            # append
            class_word_freq = data.table(class_word_freq)
            if(is.na(all_word_freq)){
                all_word_freq = class_word_freq
            }else{
                all_word_freq = rbind(all_word_freq, class_word_freq)
            }
        }
    }

    # sort
    all_word_freq = all_word_freq[
                            order(all_word_freq$count, 
                                  decreasing=T),]
     
    return(all_word_freq)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
outfile = args[1]

freq_res = make_word_freq_table()
write.table(freq_res, file=outfile, sep="\t", quote=F, row.names=F)
