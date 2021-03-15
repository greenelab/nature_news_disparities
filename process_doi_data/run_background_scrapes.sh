


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## set up paths
REF_DIR="${DIR}/../data/doi_data/downloads/"
OUT_DIR="${DIR}/../data/author_data/"

## check if the API_KEY is set
if [ -z ${SPRINGER_API_KEY+x} ]; then
    echo "API_KEY is not set, please set it or run 'setup.sh'"

else
    echo "using API_KEY:" 
    echo ${SPRINGER_API_KEY}

    echo "running API calls"
    RScript ${DIR}/springer_scripts/background_author_scrape.R ${SPRINGER_API_KEY}
    RScript ${DIR}/springer_scripts/cite_author_doi_scrape.R ${SPRINGER_API_KEY} ${REF_DIR}
    RScript ${DIR}/springer_scripts/country_doi_scrape.R ${SPRINGER_API_KEY}

    # this APi call must come AFTER cite_author_doi_scrape.R
    # it assumes that springer_cited_author_cache.tsv is fully populated 
    RScript ${DIR}/springer_scripts/cite_author_country_doi_scrape.R ${SPRINGER_API_KEY} ${REF_DIR}


    echo "processing Springer/Nature API calls"
    RScript ${DIR}/process_author_gender.R ${SPRINGER_API_KEY}
    RScript ${DIR}/process_author_country.R ${SPRINGER_API_KEY}

    echo "get number of springer article per year"
    RScript ${DIR}/springer_scripts/get_springer_articles_per_year.R ${SPRINGER_API_KEY}



fi
