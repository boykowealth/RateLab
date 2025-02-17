#' US Treasury Data
#'
#' This fucntion pulls rate data directly from the US Federal Reserve (As early as Jan-1962)
#' @return A tibble with rate data
#' @export
#' @examples
#' TREASURY_US()

TREASURY_US <- function(){
  columns <- c("Date", "US1M", "US3M", "US6M", "US1Y", "US2Y", "US3Y", "US5Y", "US7Y", "US10Y", "US20Y", "US30Y")
  
  rates <- readr::read_csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=bf17364827e38702b42a58cf8eaa3f78&lastobs=&from=&to=&filetype=csv&label=include&layout=seriescolumn&type=package") %>% 
    dplyr::slice(-c(1:5)) %>% 
    dplyr::rename_all(~ columns) %>% 
    dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    dplyr::mutate(dplyr::across(-Date, as.numeric)) %>% 
    dplyr::mutate(dplyr::across(-Date, ~ . / 100))
                  
    return(rates)
}
