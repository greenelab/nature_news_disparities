#!/usr/bin/env bash

TAG_NAME="nature_news_disparities:1.0.0"
DATA_VOLUME_NAME='nnd_data'

# default is not to do a volume mount and to do a host bind
# this is because
# we assume that the user is building the docker image
# from the git repo, so they already have all of the data 
# (assuming they had git lfs installed)
# IF they only got the image and NOT the repo, you would need to perform
# a volume mount and download the data
if [[ $1 == '--volume-bind' || $1 == '-vh' ]]; then
    shift
    VOLUME_BIND=1
    echo "Creating container with /app/data folder bound to named volume '${DATA_VOLUME_NAME}'"
fi

# check if the .env file exists
if [[ ! -f .env ]]; then
    echo "please create a .env file, a template file (.env_template) should be updated and renamed"
else
    source .env
    if [[ ${SPRINGER_API_KEY} == "REPLACE_ME" ]]; then
        echo "Your Springer key will need to be replaced for you to access the API."
        echo "This is only needed if you would like to re-scrape and re-process the data."
    fi

fi


if [[ $VOLUME_BIND -eq 1 ]]; then
    docker run --rm -v ${DATA_VOLUME_NAME}:/app/data --env-file .env -it ${TAG_NAME} /app/entrypoint.sh $@
else
    docker run --rm -v $PWD/data:/app/data --env-file .env -it ${TAG_NAME} /app/entrypoint.sh $@  
fi
