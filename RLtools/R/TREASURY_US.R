#' US Treasury Data
#'
#' This fucntion pulls rate data directly from the US Federal Reserve (As early as Jan-1962)
#' @return A tibble with rate data
#' @export
#' @examples
#' TREASURY_US()

TREASURY_US <- function(){
  columns <- c("Date", "US1M", "US3M", "US6M", "US1Y", "US2Y", "US3Y", "US5Y", "US7Y", "US10Y", "US20Y", "US30Y")
  
  rates <- readr::read_csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=bf17364827e38702b42a58cf8eaa3f78&lastobs=&from=&to=&filetype=csv&label=include&layout=seriescolumn&type=package", show_col_types = FALSE) %>% 
    dplyr::slice(-c(1:5)) %>% 
    dplyr::rename_all(~ columns) %>% 
    dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    dplyr::mutate(dplyr::across(-Date, ~ ifelse(. == "ND", NA, .))) %>% 
    dplyr::mutate(dplyr::across(-Date, readr::parse_number)) %>% 
    dplyr::mutate(dplyr::across(-Date, ~ . / 100)) %>% 
    tidyr::pivot_longer(cols = -Date, names_to = "Maturity", values_to = "Rate") %>% 
    dplyr::arrange(Maturity, Date) %>%
    dplyr::group_by(Maturity, lubridate::year(Date), lubridate::month(Date)) %>%
    dplyr::mutate(
      First_Date = dplyr::first(Date), ## This gets the first date of each new month.
      Coupon = ifelse(Maturity %in% c("US1M", "US3M", "US6M", "US1Y"), 0, ifelse(Date == First_Date, Rate, NA_real_))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Maturity) %>%
    tidyr::fill(Coupon, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Maturity, Rate, Coupon) %>% 
    dplyr::mutate(
      t2m = as.numeric(stringr::str_split_i(Maturity, 'S|M|Y', 2)),
      t2m = ifelse(stringr::str_detect(Maturity, 'M'), t2m / 12, t2m)
    ) %>% tidyr::drop_na()
                  
    return(rates)
}

