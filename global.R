#devtools::install_github('boykowealth/RateLab', subdir = "RLtools")

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
            dplyr::filter(Date >= "1970-01-01")

rates_list <- rates_df %>%  ## List of products available in portfolio (For Dropdown Menu)
              dplyr::select(Maturity) %>% unique() %>% 
              stats::setNames(., .)

start_date <<- Sys.Date() - 1825
end_date <<- Sys.Date()
