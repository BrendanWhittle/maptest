FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
##RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','plotly','leaflet','mapview','DT','lubridate','shinyWidgets','readr'), repos='https://cran.rstudio.com/')"
## fixing running as non root
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','leaflet','mapview'), repos='https://cran.rstudio.com/')"

RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('DT','lubridate','shinyWidgets','readr'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN sudo apt-get update && apt-get install -y libprotobuf-dev protobuf-compiler libv8-3.14-dev libjq-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('plotly','shiny','shinyjs','htmltools','reshape2','DT'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
# copy shiny-server config file
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY tag.rds /srv/shiny-server/tagging/
COPY app.R /srv/shiny-server/tagging/

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
