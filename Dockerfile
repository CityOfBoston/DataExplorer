FROM ubuntu:16.04

MAINTAINER Albert Lee "albert.lee@boston.gov"

# system libraries of general use
RUN apt-get update && apt-get install -y software-properties-common

RUN add-apt-repository 'deb https://mirror.ibcp.fr/pub/CRAN/bin/linux/ubuntu xenial/' 

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get install -y apt-transport-https 

RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libgeos-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev

RUN apt-get install -y r-base-core

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# basic app dependencies 
RUN R -e "install.packages('rgdal',type='source')"
RUN R -e "install.packages('rgeos',type='source')"
RUN R -e "install.packages(c('colourpicker','spatialEco','leaflet','rjson','geojsonio','shinyBS','DT','shinyjs'))"
# copy app to the image 
RUN mkdir /root/data_explorer
COPY . /root/data_explorer/
COPY Rprofile.site /usr/lib/R/etc 
EXPOSE 3838


CMD ["R", "-e shiny::runApp('/root/data_explorer')"]
