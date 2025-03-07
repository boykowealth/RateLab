#devtools::install_github('boykowealth/RateLab', subdir = "RLtools")

library(dplyr)
library(tidyverse)
library(RLtools)
library(rlang)
library(utils)
library(stringr)
library(lubridate)
library(zoo)
library(tidyquant)

library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(DT)
library(Rcpp)



Rcpp::sourceCpp('src/TreasuryPrices.cpp')
Rcpp::sourceCpp('src/TreasurySensitivities.cpp')
Rcpp::sourceCpp('src/bondprice.cpp', env = globalenv())
Rcpp::sourceCpp('src/Sensitivities.cpp', env = globalenv())
Rcpp::sourceCpp('src/CrossGamma.cpp', env = globalenv())


## Add to package 

pullsensitivity <- function(df, t2m, local_YTM, measure){
  
  if(t2m != 'portfolio'){
    
    res <- df %>% 
      dplyr::filter(YTM == local_YTM & T2M == t2m) %>% 
      dplyr::pull(!!measure)
    
    return(res)
    
  } else{
    
    res <- df %>% 
      dplyr::filter(YTM == local_YTM) %>% 
      dplyr::pull(!!measure)
    
    return(res)
  }
}

pullLocals <- function(df, x, metric) {
  
  res <-  df %>% 
    
    dplyr::filter(T2M == x) %>% 
    
    dplyr::pull(!!metric)
  
  return(res)
}

ts_y_format <- function(ts_input){
  
  if(ts_input == "Rate" || ts_input == "Volatility"){
    
    return(ggplot2::scale_y_continuous(labels = scales::percent))
    
  } else if(ts_input == "Price"){
    
    return(ggplot2::scale_y_continuous(labels = scales::dollar))
    
  }
}

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

inflation_df <- tidyquant::tq_get(
  x = 'CPIAUCSL',
  get = 'economic.data',
  from = '1959-01-01'
) %>% 
  dplyr::mutate(yoy = (price / lag(price, 12)) - 1) %>% 
  tidyr::drop_na() %>% 
  tidyr::pivot_wider(id_cols = date, names_from = symbol, values_from = yoy) %>% 
  dplyr::rename('Inflation' = CPIAUCSL)

rates_df <- TreasurySensitivities(p, 0.0001) %>% 
  as_tibble() %>%
  dplyr::left_join(datesmat, ., by = dplyr::join_by(n == Index)) %>% 
  dplyr::left_join(., inflation_df, by = dplyr::join_by(Date == date)) %>% 
  tidyr::fill(Inflation, .direction = 'down')


