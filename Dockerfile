FROM rocker/r-ver:3.5.2

MAINTAINER Elena Austin "elaustin@github.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the SYVisualization app
RUN R -e "pacman::p_load(c('leaflet', 'RColorBrewer', 'scales', 'lattice', 'dplyr', 'DT', 'data.table','ggplot2', 'ggthemes'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/visualizer
COPY visualizer /root/visualizer

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

CMD ["R", "-e shiny::runApp('/root/visualizer')"]