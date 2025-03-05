#devtools::install_github('boykowealth/RateLab', subdir = "RLtools")

library(dplyr)
library(tidyverse)
library(tidyquant)
library(RLtools)
library(rlang)
library(utils)
library(stringr)
library(lubridate)

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(Rcpp)



Rcpp::sourceCpp('src/TreasuryPrices.cpp')
Rcpp::sourceCpp('src/TreasurySensitivities.cpp')
Rcpp::sourceCpp('src/bondprice.cpp', env = globalenv())
Rcpp::sourceCpp('src/Sensitivities.cpp', env = globalenv())


## Data Collection
start_date <- Sys.Date() - 30
end_date <- Sys.Date()

rates_df <- RLtools::TREASURY_US() %>% 
            dplyr::filter(Date >= "1970-01-01") %>% 
            dplyr::mutate(n = dplyr::row_number())

datesmat <- rates_df %>% select(n, Date, Maturity)
params <- rates_df %>% select(n, c(-Date, -Maturity))
matrix <- params %>% as.matrix()
p <- TreasuryPrices(matrix, 0.0001)

rates_df <- TreasurySensitivities(p, 0.0001) %>% 
  as_tibble() %>% 
  dplyr::left_join(datesmat, ., by = dplyr::join_by(n == Index))
