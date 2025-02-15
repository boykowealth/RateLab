#' Bond Pricing Tool For Any Component
#' Specify which part of the bond calculation should be made and input the other variables.
#'
#' @param price The price of the bond.
#' @param yield The current yield of the bond.
#' @param coupon_rate The annualized coupon rate.
#' @param maturity Years to maturity.
#' @param yield The current yield of the bond.
#' @param solve_for The variable the function should solve for.
#' @return A component of bond pricing defined by the user inputs.
#' @export
#' @examples
#' BOND(yield = 0.05, coupon_rate = 0.06, maturity = 3, solve_for = "price")
#' BOND(price = 95, coupon_rate = 0.06, maturity = 3, solve_for = "yield")
#' BOND(price = 95, yield = 0.05, maturity = 3, solve_for = "coupon_rate")
#' BOND(price = 95, yield = 0.05, coupon_rate = 0.06, maturity = 3, solve_for = "face_value")
BOND <- function(price = NA, yield = NA, coupon_rate = NA, face_value = 100, maturity = NA, solve_for = "price") {
  
  if (solve_for == "price") {
    if (is.na(yield) || is.na(coupon_rate) || is.na(maturity)) {
      stop("To solve for price, please provide yield, coupon_rate, and maturity.")
    }
    coupon_payment <- face_value * coupon_rate
    price <- sum(sapply(1:maturity, function(t) {
      coupon_payment / (1 + yield)^t
    })) + face_value / (1 + yield)^maturity
    
    return(price)
    
  } else if (solve_for == "yield") {
    if (is.na(price) || is.na(coupon_rate) || is.na(maturity)) {
      stop("To solve for yield, please provide price, coupon_rate, and maturity.")
    }
    coupon_payment <- face_value * coupon_rate
    yield_func <- function(yield) {
      bond_price <- sum(sapply(1:maturity, function(t) {
        coupon_payment / (1 + yield)^t
      })) + face_value / (1 + yield)^maturity
      return(bond_price - price)
    }
    yield <- uniroot(yield_func, c(0, 1))$root
    
    return(yield)
    
  } else if (solve_for == "coupon_rate") {
    if (is.na(price) || is.na(yield) || is.na(maturity)) {
      stop("To solve for coupon_rate, please provide price, yield, and maturity.")
    }
    face_value <- ifelse(is.na(face_value), 1000, face_value)
    
    coupon_payment_func <- function(coupon_payment) {
      bond_price <- sum(sapply(1:maturity, function(t) {
        coupon_payment / (1 + yield)^t
      })) + face_value / (1 + yield)^maturity
      return(bond_price - price)
    }
    
    # Adjust interval to ensure it brackets the root
    coupon_payment <- uniroot(coupon_payment_func, c(0, price * (1 + yield)^maturity / maturity))$root
    coupon_rate <- coupon_payment / face_value
    
    return(coupon_rate)
    
  } else if (solve_for == "face_value") {
    if (is.na(price) || is.na(yield) || is.na(coupon_rate) || is.na(maturity)) {
      stop("To solve for face_value, please provide price, yield, coupon_rate, and maturity.")
    }
    coupon_payment <- face_value * coupon_rate
    face_value_func <- function(face_value) {
      bond_price <- sum(sapply(1:maturity, function(t) {
        coupon_payment / (1 + yield)^t
      })) + face_value / (1 + yield)^maturity
      return(bond_price - price)
    }
    face_value <- uniroot(face_value_func, c(0, price * 2))$root
    
    return(face_value)
    
  } else {
    stop("Invalid solve_for parameter. Please choose from 'price', 'yield', 'coupon_rate', or 'face_value'.")
  }
}  
