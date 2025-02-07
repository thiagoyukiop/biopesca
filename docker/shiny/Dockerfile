FROM rocker/shiny:4.4.1 AS builder

LABEL br.univali.acad.lema.authors="UNIVALI / Laboratório de Estudos Marinhos Aplicados - LEMA"
LABEL br.univali.acad.lema.description="Shiny server preparado para as demandas do laboratório LEMA"
LABEL br.univali.acad.lema.website="lema.acad.univali.br"
LABEL br.univali.acad.lema.source="github.com/rodrigosantana/lema/shiny"
LABEL br.univali.acad.lema.version="v1.0.0"
LABEL br.univali.acad.lema.release-date="2024-08-20"

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -y \
 && apt upgrade -y \
 && apt install  -y \
        libcurl4-openssl-dev \
        libssl-dev \
        libpq-dev \
        libudunits2-dev \
        libproj-dev \
        libgdal-dev \
        libglpk-dev 

RUN R -e 'install.packages("shiny", dependencies=TRUE)'
RUN R -e 'install.packages("shinydashboard", dependencies=TRUE)'
RUN R -e 'install.packages("shinydashboardPlus", dependencies=TRUE)'
RUN R -e 'install.packages("leaflet", dependencies=TRUE)'
RUN R -e 'install.packages("ggplot2", dependencies=TRUE)'
RUN R -e 'install.packages("dplyr", dependencies=TRUE)'
RUN R -e 'install.packages("plotly", dependencies=TRUE)'
RUN R -e 'install.packages("zoo", dependencies=TRUE)'
RUN R -e 'install.packages("tidyverse", dependencies=TRUE)'
RUN R -e 'install.packages("digest", dependencies=TRUE)'
RUN R -e 'install.packages("DT", dependencies=TRUE)'
RUN R -e 'install.packages("shinyjs", dependencies=TRUE)'
RUN R -e 'install.packages("leaflet.extras", dependencies=TRUE)'
RUN R -e 'install.packages("janitor", dependencies=TRUE)'
RUN R -e 'install.packages("caret", dependencies=TRUE)'
RUN R -e 'install.packages("mlbench", dependencies=TRUE)'
RUN R -e 'install.packages("e1071", dependencies=TRUE)'
RUN R -e 'install.packages("naniar", dependencies=TRUE)'
RUN R -e 'install.packages("corrplot", dependencies=TRUE)'
RUN R -e 'install.packages("FactoMineR", dependencies=TRUE)'
RUN R -e 'install.packages("factoextra", dependencies=TRUE)'
RUN R -e 'install.packages("randomForest", dependencies=TRUE)'
RUN R -e 'install.packages("ROCR", dependencies=TRUE)'
RUN R -e 'install.packages("ggvis", dependencies=TRUE)'
RUN R -e 'install.packages("missForest", dependencies=TRUE)'
RUN R -e 'install.packages("mice", dependencies=TRUE)'
RUN R -e 'install.packages("DescTools", dependencies=TRUE)'
RUN R -e 'install.packages("DataExplorer", dependencies=TRUE)'
RUN R -e 'install.packages("remotes", dependencies=TRUE)'
RUN R -e 'install.packages("sf", dependencies=TRUE)'
RUN R -e 'install.packages("raster", dependencies=TRUE)'
RUN R -e 'install.packages("rasterVis", dependencies=TRUE)'
RUN R -e 'install.packages("reshape", dependencies=TRUE)'
RUN R -e 'install.packages("tidyr", dependencies=TRUE)'
RUN R -e 'install.packages("cowplot", dependencies=TRUE)'
RUN R -e 'install.packages("rnaturalearth", dependencies=TRUE)'
RUN R -e 'install.packages("ggspatial", dependencies=TRUE)'
RUN R -e 'install.packages("sdm", dependencies=TRUE)'
RUN R -e 'install.packages("usdm", dependencies=TRUE)'
RUN R -e 'install.packages("SDMtune", dependencies=TRUE)'
RUN R -e 'install.packages("dismo", dependencies=TRUE)'
RUN R -e 'install.packages("viridis", dependencies=TRUE)'
RUN R -e 'install.packages("pacman", dependencies=TRUE)'
RUN R -e 'install.packages("shiny.i18n", dependencies=TRUE)'
RUN R -e 'install.packages("RColorBrewer", dependencies=TRUE)'
RUN R -e 'install.packages("fresh", dependencies=TRUE)'
RUN R -e 'install.packages("nlstools", dependencies=TRUE)'
RUN R -e 'install.packages("rintrojs", dependencies=TRUE)'

RUN rm -rf /tmp/downloaded_packages \
 && strip /usr/local/lib/R/site-library/*/libs/*.so




FROM builder 

WORKDIR /srv/shiny-server

# removes default shiny apps (lazy way)
RUN rm -rf /srv/shiny-server/*

USER shiny 
COPY ./src /srv/shiny-server/
RUN rm -rf /srv/shiny-server/.git

CMD ["shiny-server"]

