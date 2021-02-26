


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## set up paths
REF_DIR="${DIR}/../data/doi_data/downloads/"

## check if the API_KEY is set
if [ -z ${SPRINGER_API_KEY+x} ]; then
    echo "API_KEY is not set, please set it or run 'setup.sh'"

else
    echo "using API_KEY:" 
    echo ${SPRINGER_API_KEY}

    echo "running API calls"
    RScript ${DIR}/background_author_scrape.R ${SPRINGER_API_KEY}
    RScript ${DIR}/cite_author_doi_scrape.R ${SPRINGER_API_KEY} ${REF_DIR}
    RScript ${DIR}/country_doi_scrape.R ${SPRINGER_API_KEY}

    echo "processing Springer API calls"


    echo "processing Nature scrape"


fi


cited_dois_dir="/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/doi_data/downloads/"
nature_dir="/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/downloads"
outdir="/Users/natalie/Documents/projects/greenelab/checkouts/nature_news_disparities/data/author_data/"