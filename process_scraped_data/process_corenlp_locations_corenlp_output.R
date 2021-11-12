require(jsonlite)
require(data.table)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))

#' Read in the location informations from the coreNLP JSON output
#'
#' @param corenlp_output_dir The directory containing all JSON result files, one per article
#' @return a dataframe of the location information
read_result_files <- function(corenlp_output_dir){
    
    json_res_files = list.files(corenlp_output_dir, pattern=".txt.json", full.names = TRUE)
    
    all_locations = NA
    for(curr_file in json_res_files[608:length(json_res_files)]){
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        ## get location info
        print(file_id)
        loc_df = get_osm_locations(json_res)

        ## format final df
        if(!is.na(loc_df)){
            loc_df = data.frame(file_id=file_id, loc_df)
        }else{
            next
        }
        all_locations = rbind(all_locations, loc_df)

        # clean up in case memory issues 
        gc()

    }

    all_locations = all_locations[-1,]

    return(all_locations)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outfile = args[2]

loc_res = read_result_files(corenlp_output_dir)
write.table(loc_res, file=outfile, sep="\t", quote=F, row.names=F)

rm(list=ls())
gc()
