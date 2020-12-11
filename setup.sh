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
    export CLASSPATH="$CLASSPATH:${DIR}/stanford-corenlp-${CORENLP_VERSION})/*"
    echo "New classpath: $CLASSPATH"
fi
