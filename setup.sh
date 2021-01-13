#!/usr/bin/env bash

#### delete this later
#export CLASSPATH=$CLASSPATH:/Users/natalie/Documents/projects/greenelab/stanford-corenlp-4.2.0/*:

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CORENLP_VERSION='4.2.0'


# check for coreNLP
echo "Checking for stanford-corenlp-${CORENLP_VERSION}..."
if [[ ! -d ./stanford-corenlp-${CORENLP_VERSION} && "$CLASSPATH" != *"stanford-corenlp-${CORENLP_VERSION}"* ]]; then
    read -r -p "CoreNLP doesn't exist, download? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            curl -L -O "http://nlp.stanford.edu/software/stanford-corenlp-${CORENLP_VERSION}.zip" \
                && unzip stanford-corenlp-${CORENLP_VERSION}.zip \
                && rm stanford-corenlp-${CORENLP_VERSION}.zip
            ;;
        *)
            # just abort for now
            ;;
    esac
else
    echo "stanford coreNLP found"
fi


if [[ "$CLASSPATH" != *"stanford-corenlp-${CORENLP_VERSION}"* ]]; then
    export CLASSPATH="$CLASSPATH:${DIR}/stanford-corenlp-${CORENLP_VERSION}/*"
    echo "New classpath: $CLASSPATH"
fi


# get reference data from nature_index
# check for it first
echo "Checking for nature index organization reference file..."
if [[ ! -f ./data/reference_data/nature_index_export.csv ]]; then
    read -r -p "Nature Index reference data doesn't exist, download? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            curl -L "https://www.natureindex.com/institution-outputs-export/All/global/All/score/1" \
                -o ${DIR}/data/reference_data/nature_index_export.csv
            ;;
        *)
            # just abort for now
            ;;
    esac
else
    echo "Nature Index reference data found"
fi


# get reference data of state and country codes
# check for it first
echo "Checking for country and state info reference file..."
if [[ ! -f ./data/reference_data/cdh_country_codes.txt | ! -f ./data/reference_data/cdh_state_codes.txt ]]; then
    read -r -p "Nature Index reference data doesn't exist, download? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            curl -L "https://gist.github.com/nrosed/af41858718a1bc30f0323d95916b5c4e/raw/2930f0c786a32c873ddcd7d51defbf6ca0846600/cdh_country_codes.txt" \
                -o ${DIR}/data/reference_data/cdh_country_codes.txt
            curl -L "https://gist.github.com/nrosed/af41858718a1bc30f0323d95916b5c4e/raw/2930f0c786a32c873ddcd7d51defbf6ca0846600/cdh_state_codes.txt" \
                -o ${DIR}/data/reference_data/cdh_state_codes.txt
            ;;
        *)
            # just abort for now
            ;;
    esac
else
    echo "Country and State reference data found"
fi


# get reference data of gender names
# check for it first
echo "Checking for genderize.io reference file..."
if [[ ! -f ./data/reference_data/genderize.tsv ]]; then
    read -r -p "genderize.io reference data doesn't exist, download? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            curl -L "https://github.com/greenelab/iscb-diversity/raw/2beece62588d52dc30229fd65f25ddd523fa955e/data/gender/genderize.tsv" \
                -o ${DIR}/data/reference_data/genderize.tsv
            ;;
        *)
            # just abort for now
            ;;
    esac
else
    echo "genderize.io reference data found"
fi

