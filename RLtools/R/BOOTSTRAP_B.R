#' Bootstrap Yield Curve from Bond Prices and Coupons
#'
#' This function bootstraps a zero-coupon yield curve from bond prices and coupon rates.
#' It assumes annual coupons and that the bond prices are known.
#'
#' @param data A data frame with bond prices, coupon rates, and maturities.
#' @return A tibble with maturities and zero rates.
#' @export
#' @examples
#'bonds <- data.frame(
#'           Maturity = c(1, 2, 3),
#'           Price = c(950, 920, 880),
#'           Coupon = c(0, 50, 60),
#'           FaceValue = 1000
#'           )
#' BOOTSTRAP_B(bond_data = bond_data)

BOOTSTRAP_B <- function(bond_data) {
  
  zero_rates <- numeric(length = nrow(bond_data))
  
  calculate_first_zero_rate <- function(price, face_value) {
    r <- (face_value / price) - 1
    return(r)
  }
  
  calculate_zero_rate <- function(bond, prev_rates) {
    price <- bond$Price
    coupon <- bond$Coupon
    face_value <- bond$FaceValue
    maturity <- bond$Maturity
    
    pv_coupons <- sum(purrr::map_dbl(1:(maturity - 1), function(t) {
      coupon / (1 + prev_rates[t])^t
    }))
    
    r <- ((face_value + coupon - pv_coupons) / price)^(1 / maturity) - 1
    return(r)
  }
  
  for (i in 1:nrow(bond_data)) {
    if (i == 1) {
      zero_rates[i] <- calculate_first_zero_rate(bond_data$Price[i], bond_data$FaceValue[i])
    } else {
      zero_rates[i] <- calculate_zero_rate(bond_data[i, ], zero_rates)
    }
  }
  
  bond_data$ZeroRate <- zero_rates
  
  return(bond_data %>% dplyr::select(Maturity, ZeroRate))
}


