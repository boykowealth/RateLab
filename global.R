devtools::install_github('boykowealth/RateLab', subdir = "RLtools")

library(dplyr)
library(tidyverse)
library(tidyquant)
library(RLtools)
library(rlang)

library(shiny)
library(bslib)
library(shinyalert)
library(plotly)
library(DT)


## Data Collection
rates_df <- RLtools::TREASURY_US() %>% 
            dplyr::filter(Date >= "1970-01-01") %>%
            dplyr::mutate(dplyr::across(-Date, ~ifelse(is.na(.), 0, .)))




