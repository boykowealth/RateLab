#' Bootstrap Yield Curve from Swap Rates
#'
#' This function bootstraps a zero-coupon yield curve from swap rates.
#' It assumes annualized par rates
#'
#' @param data A data frame with swap rates and maturities.
#' @return A tibble with maturities and zero rates.
#' @export
#' @examples
#' swap_data <- tibble(maturity = c(1, 2, 3, 4),
#'                     rate = c(0.03, 0.035, 0.04, 0.045)
#' BOOTSTRAP_S(data=swap_data)

BOOTSTRAP_S <- function(data) {
  
  n <- nrow(data)
  zero_rates <- numeric(n)
  discount_factors <- numeric(n)
  
  for (i in 1:n) {
    maturity <- data$maturity[i]
    par <- data$rate[i]
    
    # Formulas As Per Alex During, Fixed Income Trading and Risk Management (2021), Page 194
    # This Satisfies No Arbitrage Conditions.
    
    if (i == 1) {
      discount_factors[i] <- 1 / (1 + par) 
    } else {
      sum_df <- sum(discount_factors[1:(i-1)])
      discount_factors[i] <- (1 - par * sum_df) / (1 + par)
    }
    zero_rates[i] <- sqrt(1 / discount_factors[i]) - 1
  }
  
  result <- dplyr::tibble(
    maturity = data$maturity,
    zero_rate = zero_rates
  )
  
  return(result)
}
