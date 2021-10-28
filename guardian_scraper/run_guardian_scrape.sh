
## get directory of this script for relative paths
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


## check if the API_KEY is set
if [ -z ${GUARDIAN_API_KEY+x} ]; then
    echo "API_KEY is not set, please set it or run 'setup.sh'"

else
    echo "using API_KEY:" 
    echo ${GUARDIAN_API_KEY}

    echo "running API calls"
    RScript ${DIR}/guardian_scrape.R ${GUARDIAN_API_KEY}
    

fi
