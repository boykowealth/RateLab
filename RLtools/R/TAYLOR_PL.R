#' Taylor Series Expansion Profit/Loss
#'
#' This function measures profit and loss given Delta (Duration), Gamma (Convexity), and Speed
#'
#' @param delta Delta/Durration value (First-Order)
#' @param gamma Gamma/Convexity value (Second-Order)
#' @param theta Theta/Speed value (Third-Order)
#' @return The Profit/Loss Given The Taylor Series Expansion
#' @export
#' @examples
#' vasicek(LT_mean = 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)

TAYLOR_PL <- function(delta, gamma, theta){
  
  results <- delta + (0.5*(gamma^2) + theta) ## This Describes The Modern Risk Framework For Bond P/L
  
  return(results)
  
}