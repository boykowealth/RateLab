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

Rcpp::sourceCpp('src/TreasuryPrices.cpp')
Rcpp::sourceCpp('src/TreasurySensitivities.cpp')

## Data Collection
start_date <- Sys.Date() - 30
end_date <- Sys.Date()

rates_df <- RLtools::TREASURY_US() %>% 
            dplyr::filter(Date >= "1970-01-01") %>% 
            dplyr::mutate(n = dplyr::row_number())

datesmat <- dat %>% select(n, Date, Maturity)
params <- dat %>% select(n, c(-Date, -Maturity))
matrix <- params %>% as.matrix()
p <- TreasuryPrices(matrix, 0.0001)

rates_df <- TreasurySensitivities(p, 0.0001) %>% 
  as_tibble() %>% 
  dplyr::left_join(datesmat, ., by = dplyr::join_by(n == Index))
