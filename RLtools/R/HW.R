#' Hull-White One-Factor Model Interest Rate Simulation
#'
#' This function simulates the Hull-White one-factor model, which allows for a time-dependent mean.
#' The model is widely used in fixed-income derivatives pricing (e.g., swaptions, bond options).
#'
#' @param LT_mean Long-term mean function (constant or function of time)
#' @param rate Initial interest rate
#' @param delta_T Time step
#' @param sigma Volatility of interest rates
#' @param theta Mean reversion speed
#' @param T2M Time to maturity (total time horizon)
#' @param nsims Number of simulation paths
#' @return A tibble with simulated interest rate paths
#' @export
#' @examples
#' HW(LT_mean = function(t) 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)

HW <- function(LT_mean, rate, delta_T, sigma, theta, T2M, nsims){
  
  periods <- T2M / delta_T
  rates <- matrix(0, nrow = periods, ncol = nsims)
  rates[1, ] <- rate
  
  diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(delta_T)), ncol = nsims, nrow = periods)
  
  for (t in 2:periods) {
    time <- (t-1) * delta_T
    drift_term <- theta * (LT_mean(time) - rates[t-1, ]) * delta_T
    diffusion_term <- sigma * diffusion[t, ]
    rates[t, ] <- rates[t-1, ] + drift_term + diffusion_term
  }
  
  rates <- dplyr::as_tibble(rates, .name_repair = "minimal")
  names(rates) <- paste("sim", 1:nsims, sep = "")
  rates <- rates %>% mutate(t = seq(0, T2M - delta_T, delta_T)) %>% select(t, everything())
  
  return(rates)
}

HW(LT_mean = function(t) 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)
