FROM rocker/shiny:latest

MAINTAINER Fabrice Zaoui "fabrice.zaoui@edf.fr"

# install ssl
RUN sudo apt-get update; exit 0
RUN sudo apt-get install -y libssl-dev
RUN sudo apt-get install -y libcurl4-openssl-dev

# Java
RUN sudo apt-get install -y default-jdk libicu-dev libbz2-dev liblzma-dev && \
     R -e "install.packages('rJava')"

# XML
RUN sudo apt-get install -y libxml2-dev

# install additional packages
RUN R -e "install.packages(c('xml2', 'xlsx', 'DT', 'plotly', 'rmarkdown', 'shinyjs', 'leaflet', 'knitr', 'kableExtra'), repos='https://cran.rstudio.com/')"

# copy shiny-server config file
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

COPY app/ /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
# COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
