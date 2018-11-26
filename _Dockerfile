FROM openanalytics/r-base

MAINTAINER Luis Sano-Espinosa "luis.sano@boston.gov"

# system libraries of general use
RUN apt-get update && apt-get install -f  -y \
sudo \
pandoc \
pandoc-citeproc \
libcairo2-dev \
libxt-dev \
libssh2-1-dev \
libgeos-dev \
libgdal-dev \
libproj-dev \
libgdal1i \
libcurl4-openssl-dev \
libv8-dev \
libgdal1-dev \
libprotobuf-dev \
protobuf-compiler
#libv8-3.14-dev

ARG BASE_URL
ENV BASE_URL=$BASE_URL



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
