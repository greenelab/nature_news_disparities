require(jsonlite)
require(data.table)
require(here)

proj_dir = here()
source(paste(proj_dir, "/utils/scraper_processing_utils.R", sep=""))


read_result_files <- function(corenlp_output_dir){
    
    json_res_files = list.files(corenlp_output_dir, pattern=".txt.json", full.names = TRUE)
    
    all_locations = NA
    for(curr_file in json_res_files){
        
        file_id = basename(curr_file)
        file_id = substr(file_id, 1, nchar(file_id)-9)
        
        json_res = fromJSON(curr_file)

        ## get location info
        print(file_id)
        loc_df = get_locations(json_res)

        ## format final df
        if(!is.na(loc_df)){
            loc_df = data.frame(file_id=file_id, loc_df)
        }else{
            next
        }
        all_locations = rbind(all_locations, loc_df)

    }

    all_locations = all_locations[-1,]
    all_locations$country = unlist(all_locations$location)
    all_locations$location = NULL
    all_locations = apply(all_locations, 2, unlist)

    return(all_locations)
}



### read in arguments
args = commandArgs(trailingOnly=TRUE)
corenlp_output_dir = args[1]
outfile = args[2]

loc_res = read_result_files(corenlp_output_dir)
write.table(loc_res, file=outfile, sep="\t", quote=F, row.names=F)
