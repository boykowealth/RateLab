FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    git

RUN R -e "install.packages(c('devtools', 'dplyr', 'tidyverse', 'rlang', 'utils', 'stringr', 'lubridate', 'shiny', 'bslib', 'DT', 'ggplot2', 'rcpp', 'tidyquant', 'plotly'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"
RUN R -e "devtools::install_github('boykowealth/RateLab', subdir = 'RLtools')"

RUN git clone https://github.com/boykowealth/ratelab.git /srv/shiny-server/ratelab
RUN chown -R shiny:shiny /srv/shiny-server/ratelab

EXPOSE 3838

#CMD ["/init"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/ratelab/', host = '0.0.0.0', port = 3838)"]
