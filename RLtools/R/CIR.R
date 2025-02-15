#' Cox-Ingersoll-Ross (CIR) Interest Rate Simulation
#'
#' This function simulates the CIR model, which is commonly used in fixed income pricing.
#' It ensures that interest rates remain non-negative due to the square-root diffusion term.
#'
#' @param LT_mean Long-term mean interest rate (mu)
#' @param rate Initial interest rate
#' @param delta_T Time step
#' @param sigma Volatility of interest rates
#' @param theta Mean reversion speed
#' @param T2M Time to maturity (total time horizon)
#' @param nsims Number of simulation paths
#' @return A tibble with simulated interest rate paths
#' @export
#' @examples
#' CIR(LT_mean = 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)

CIR <- function(LT_mean, rate, delta_T, sigma, theta, T2M, nsims){
  
  periods <- T2M / delta_T
  rates <- matrix(0, nrow = periods, ncol = nsims)
  rates[1, ] <- rate
  
  diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(delta_T)), ncol = nsims, nrow = periods)
  
  for (t in 2:periods) {
    drift_term <- theta * (LT_mean - rates[t-1, ]) * delta_T
    diffusion_term <- sigma * sqrt(pmax(rates[t-1, ], 0)) * diffusion[t, ]
    rates[t, ] <- pmax(rates[t-1, ] + drift_term + diffusion_term, 0)
  }
  
  rates <- dplyr::as_tibble(rates, .name_repair = "minimal")
  names(rates) <- paste("sim", 1:nsims, sep = "")
  rates <- rates %>% dplyr::mutate(t = seq(0, T2M - delta_T, delta_T)) %>% select(t, dplyr::everything())
  
  return(rates)
}

CIR(LT_mean = 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)
