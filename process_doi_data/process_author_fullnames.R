
require(jsonlite)
require(data.table)
require(humaniformat)
require(here)

proj_dir = here()
source(file.path(proj_dir, "/utils/scraper_processing_utils.R"))
source(file.path(proj_dir, "/process_doi_data/springer_scripts/springer_scrape_utils.R"))



#' Read all json files to do the gender prediction
#' the nature background authors
#' @param nature_dir, directory containing scraped nature JSON output 
#' 
process_all_author_fullnames <- function(nature_dir, cited_dois_dir, outdir){

    # we have 3 source files for author info
    # springer background authorship
    springer_author_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_bg_author_cache.tsv")
    springer_author_df = data.frame(fread(springer_author_file))

    # springer cited authorship
    cited_author_file = file.path(proj_dir, 
                                    "/data/reference_data/springer_cited_author_cache.tsv")
    cited_author_df = data.frame(fread(cited_author_file))
    cited_author_df = subset(cited_author_df, !is.na(authors))
    cited_author_df = subset(cited_author_df, select=-c(year))
    cited_doi_df = get_ref_dois(cited_dois_dir)
    cited_author_df = merge(cited_author_df, cited_doi_df[,c("doi", "year", "file_id")], by=c("doi")) ### are some missing?

    # then all the nature articles
    nature_author_df = read_nature_author_json_files(nature_dir)

    # now process them to get the first and last authors
    cited_author_df = format_authors(cited_author_df, use_fullname=T)
    springer_author_df = format_authors(springer_author_df, use_fullname=T)
    nature_author_df = format_authors(nature_author_df, use_fullname=T)

    # remove any blank authors
    cited_author_df = subset(cited_author_df, author != "")
    springer_author_df = subset(springer_author_df, author != "")
    nature_author_df = subset(nature_author_df, author != "")

    cited_author_df$corpus = "naturenews_citations"
    springer_author_df$corpus = "springer_articles"
    nature_author_df$corpus = "nature_articles"

    # we care about the file_id that the article was cited in
    springer_author_df$file_id = springer_author_df$doi

    col_ids = c("year", "author_pos", "author", "file_id", "corpus")
    all_author_df = Reduce(rbind, list(cited_author_df[,col_ids],
                                        springer_author_df[,col_ids],
                                        nature_author_df[,col_ids]))

    all_author_file = file.path(outdir, "all_author_fullname.tsv")
    write.table(all_author_df, file=all_author_file, sep="\t", quote=F, row.names=F)


}

### read in arguments
args = commandArgs(trailingOnly=TRUE)
nature_dir = args[1]
cited_dois_dir = args[2]
outdir = args[3]

process_all_author_gender(nature_dir, cited_dois_dir, outdir)

