library(jsonlite)
library(data.table)
library(dplyr)

read_corenlp_quote_files <- function(corenlp_file){
    
    corenlp_df = data.frame(fread(corenlp_file, header=T))
    colnames(corenlp_df)[which(colnames(corenlp_df)=="full_name")] = "est_speaker"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="gender")] = "est_gender"

    corenlp_df$est_speaker[which(is.na(corenlp_df$est_speaker))] = "NO_EST"
    corenlp_df$est_gender[which(is.na(corenlp_df$est_gender))] = "NO_EST"


    return(corenlp_df)
}

read_benchmark_quote_file <- function(gold_file){
    
    gold_df = data.frame(fread(gold_file, header=T))
    colnames(gold_df)[which(colnames(gold_df)=="full_name")] = "true_speaker"
    colnames(gold_df)[which(colnames(gold_df)=="gender")] = "true_gender"
    return(gold_df)

}


read_corenlp_location_files <- function(corenlp_file){
    
    corenlp_df = data.frame(fread(corenlp_file, header=T))
    colnames(corenlp_df)[which(colnames(corenlp_df)=="country")] = "est_country"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="un_region")] = "est_un_region"
    colnames(corenlp_df)[which(colnames(corenlp_df)=="un_subregion")] = "est_un_subregion"

    corenlp_df$est_country[which(is.na(corenlp_df$est_country))] = "NO_EST"
    corenlp_df$est_un_region[which(is.na(corenlp_df$est_un_region))] = "NO_EST"
    corenlp_df$est_un_subregion[which(is.na(corenlp_df$est_un_subregion))] = "NO_EST"


    return(corenlp_df)
}

read_benchmark_location_file <- function(bm_loc_file){
    
    gold_df = data.frame(fread(bm_loc_file, header=T))
    colnames(gold_df)[which(colnames(gold_df)=="country")] = "true_country"
    colnames(gold_df)[which(colnames(gold_df)=="un_region")] = "true_un_region"
    colnames(gold_df)[which(colnames(gold_df)=="un_subregion")] = "true_un_subregion"
    return(gold_df)

}


make_comparison <- function(gold_df, res_df){
    
    # join the df to make comparison
    compare_df = merge(gold_df, res_df, by=c("file_id", "quote"), all.x=T)
    
    #check if the predicted speaker is contained within the true speaker
    speaker_idx = which(colnames(compare_df) == "est_speaker")
    true_speaker_idx = which(colnames(compare_df) == "true_speaker")
    speaker_match = apply(compare_df, 1, 
                          function(x) grepl(x[speaker_idx], 
                                            x[true_speaker_idx], 
                                            fixed=T)) 
    
    compare_df$is_speaker_correct = speaker_match



    gender_idx = which(colnames(compare_df) == "est_gender")
    true_gender_idx = which(colnames(compare_df) == "true_gender")
    gender_match = apply(compare_df, 1, 
                        function(x) x[gender_idx] == x[true_gender_idx]) 
    
    compare_df$is_gender_correct = gender_match
  
    return(compare_df)
    
}

evaluate_gender_speaker <- function(compare_df){

    eval_df = subset(compare_df, !is.na(true_gender))
    eval_df = subset(eval_df, true_gender!="NOT_CLEAR")
    
    # overall accuracy
    overall_gender_acc = sum(eval_df$is_gender_correct) / nrow(eval_df)
    
    ## get per DOC stats
    # perdocument accuracy
    per_doc_acc = eval_df %>%
                    group_by(file_id) %>%
                    summarize(acc = sum(is_gender_correct) / length(is_gender_correct))
    per_doc_acc = data.frame(per_doc_acc)
    
}

evaluate_speaker_attrib <- function(compare_df, outfile){
    
    eval_df = subset(compare_df, !is.na(true_speaker))
    eval_df = subset(eval_df, true_speaker!="NOT_CLEAR")
    
    # overall accuracy
    overall_speaker_acc = sum(eval_df$is_speaker_correct) / nrow(eval_df)
    
    ## get per DOC stats
    # perdocument accuracy
    per_doc_acc = eval_df %>%
                    group_by(file_id) %>%
                    summarize(acc = sum(is_speaker_correct) / length(is_speaker_correct))
    per_doc_acc = data.frame(per_doc_acc)
    
    # document stats
    num_quotes = data.frame(table(eval_df$file_id))
    colnames(num_quotes) = c("file_id", "num_quotes")
    per_doc_acc = merge(per_doc_acc, num_quotes)
    
    
}

basic_doc_stats <- function(gold_df, year_idx_file){


    eval_df = subset(gold_df, !is.na(true_gender))
    eval_df = subset(eval_df, true_gender!="NOT_CLEAR")


    year_df = data.frame(fread(year_idx_file))
    eval_df = merge(year_df, eval_df)

    ## get per year stats
    # perdocument accuracy
    per_year_gender = eval_df %>%
                    group_by(year) %>%
                    summarize(percent_male = sum(true_gender=="MALE") / length(true_gender))
    per_year_gender = data.frame(per_year_gender)



}

gold_quote_file = "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/process_scraped_data/../data/benchmark_data/benchmark_quote_table_hand_annotated.tsv"
corenlp_quote_file = "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/process_scraped_data/../data/benchmark_data/benchmark_quote_table_raw.tsv"

gold_loc_file = "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/process_scraped_data/../data/benchmark_data/benchmark_location_table_hand_annotated.tsv"
corenlp_loc_file = "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/process_scraped_data/../data/benchmark_data/benchmark_location_table_raw.tsv"

year_idx_file = "/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/process_scraped_data/../data/benchmark_data/coreNLP_input/fileID_year.tsv"

