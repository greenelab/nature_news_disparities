FROM rocker/r-ver:4.0.3

# install git-lfs, for copying in giant files later on
# also install python + pip + java
RUN \
  apt-get update && \
  apt-get install -y \
    curl git git-lfs python3 python3-pip openjdk-8-jdk \
    pandoc qpdf libcurl4-openssl-dev libmagick++-6.q16-dev libudunits2-dev libpoppler-cpp-dev \
    && \
  git lfs install

WORKDIR /app

# install R deps, this is the snapshotted version
# from when the project was being developed
# WHEN is defined in build_image.sh
ARG WHEN
RUN R -e "options(repos = \
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/${WHEN}')); \
  install.packages(c( \
    'RColorBrewer', 'RCurl', 'caret', 'data.table', 'dplyr', 'genderizeR', \
    'ggplot2', 'ggpubr', 'ggrepel', 'here', 'humaniformat', 'jsonlite', 'magick', \
    'pdftools', 'pheatmap', 'readr', 'stringdist', 'stringr', 'textclean', 'tidyr', 'tidytext', \
    'tmaptools' \
  ))"

# install knitr
RUN R -e "options(repos = \
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/${WHEN}')); \
  install.packages('knitr', dependencies = TRUE)"

# install python deps
COPY ./requirements.txt /tmp
RUN pip install -r /tmp/requirements.txt

# copy in data that's checked for by setup.sh before it downloads stuff
COPY ./data/ ./data/
COPY ./setup.sh ./setup.sh
# download data deps (e.g., corenlp models), typically unnecessary
RUN /bin/bash ./setup.sh -y

# copy in the rest of the app code
COPY . ./

# disable ghostscript <9.26 security patch that removes image writing(!)
# also bump up memory, disk, etc. allocations b/c figure 3 is large
RUN sed -i '/disable ghostscript format types/,+6d' /etc/ImageMagick-6/policy.xml && \
    sed -i -E 's/name="memory" value=".+"/name="memory" value="8GiB"/g' /etc/ImageMagick-6/policy.xml && \
    sed -i -E 's/name="map" value=".+"/name="map" value="8GiB"/g' /etc/ImageMagick-6/policy.xml && \
    sed -i -E 's/name="area" value=".+"/name="area" value="8GiB"/g' /etc/ImageMagick-6/policy.xml && \
    sed -i -E 's/name="disk" value=".+"/name="disk" value="8GiB"/g' /etc/ImageMagick-6/policy.xml

# preserve the contents of /app/data in a persistent volume
# (e.g., since we'll possibly be performing the expensive work of downloading from git lfs)
# this isn't used if you are doing the host bind (which we do by default)
VOLUME /app/data

# establish entrypoint
CMD ["/app/entrypoint.sh"]
