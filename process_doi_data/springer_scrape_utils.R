require(tidyr)
require(jsonlite)
require(data.table)

require(here)

proj_dir = here()
ref_data_dir = paste(proj_dir, "/data/reference_data/", sep = "")
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))



#' private internal method that queries springer
#' 
#' @param string; URL formatted for springer API
#' @return OSM JSON response
internal_springer_query <- function(url_search){

    # block augmented from here: 
    # https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    out <- tryCatch(
        {
            fromJSON(getURL(url_search))
        },
        error=function(cond) {
            message(paste("query error:", url_search))
            # Choose a return value in case of error
            return(NA)
        }
    )    
    return(out)
}
