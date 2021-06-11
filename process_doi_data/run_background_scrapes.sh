


## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

## set up paths
REF_DIR="${DIR}/../data/doi_data/downloads/"
QUOTE_DIR="${DIR}/../data/scraped_data/"
NATURE_DIR="${DIR}/../data/author_data/downloads/"
OUT_DIR="${DIR}/../data/author_data/"
SPRINGER_BG_AUTHOR="${DIR}/../data/reference_data/springer_bg_author_cache.tsv"
MODEL_DIR="${DIR}/../name_lstm_models/"

## check if the API_KEY is set
if [ -z ${SPRINGER_API_KEY+x} ]; then
    echo "API_KEY is not set, please set it or run 'setup.sh'"

else
    echo "using API_KEY:" 
    echo ${SPRINGER_API_KEY}

    echo "running API calls"
    RScript ${DIR}/springer_scripts/background_author_scrape.R ${SPRINGER_API_KEY}
    RScript ${DIR}/springer_scripts/cite_author_doi_scrape.R ${SPRINGER_API_KEY} ${REF_DIR}

    # this APi call must come AFTER background_author_scrape.R
    # it assumes that springer_bg_author_cache.tsv is fully populated 
    RScript ${DIR}/springer_scripts/get_springer_random_country_scrape.R ${SPRINGER_API_KEY} ${SPRINGER_BG_AUTHOR}

    # this APi call must come AFTER cite_author_doi_scrape.R
    # it assumes that springer_cited_author_cache.tsv is fully populated 
    RScript ${DIR}/springer_scripts/cite_author_country_doi_scrape.R ${SPRINGER_API_KEY} ${REF_DIR} ${OUT_DIR}


    echo "processing Springer/Nature API calls"
    RScript ${DIR}/process_author_gender.R ${NATURE_DIR} ${REF_DIR} ${OUT_DIR}
    RScript ${DIR}/process_author_country.R ${NATURE_DIR} ${REF_DIR} ${OUT_DIR}

    RScript ${DIR}/process_author_fullnames.R ${NATURE_DIR} ${REF_DIR} ${OUT_DIR}
    RScript ${DIR}/process_speaker_fullnames.R ${QUOTE_DIR} ${OUT_DIR}

    conda activate nature_news_disparities
    python ${DIR}/springer_scripts/process_author_fullnames.py -m ${MODEL_DIR} \
                                            -n ${OUT_DIR}/all_author_fullname.tsv \
                                            -o ${OUT_DIR}/all_author_fullname_pred.tsv

    python ${DIR}/springer_scripts/process_author_fullnames.py -m ${MODEL_DIR} \
                                            -n ${OUT_DIR}/all_speaker_fullname.tsv \
                                            -o ${OUT_DIR}/all_speaker_fullname_pred.tsv


    python ${DIR}/springer_scripts/process_author_fullnames.py -m ${MODEL_DIR} \
                                            -n ${OUT_DIR}/all_mentioned_fullname.tsv \
                                            -o ${OUT_DIR}/all_mentioned_fullname_pred.tsv


fi
