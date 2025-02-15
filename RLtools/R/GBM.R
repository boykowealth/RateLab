#' Geometric Brownian Motion Simulation
#'
#' This function simulates a Geometric Brownian Motion.It should not be used in interest rate models.
#' Instead it can be used in:
#'  Inflation-linked bonds
#'  Commodity-linked bonds
#'  Credit spreads in high-yield debt
#'  Convertible bonds (stock modeling component)
#'
#' @param spot Initial spot price
#' @param drift Drift rate
#' @param sigma Volatility
#' @param T2M Time to maturity
#' @param delta_T Time step
#' @param nsims Number of simulations
#' @return A tibble with simulated paths
#' @export
#' @examples
#' GBM(spot=10, drift=0, sigma=0.2, T2M=1, delta_T=1/12, nsims=2)

GBM <- function(spot, drift, sigma, T2M, delta_T, nsims){
  
  periods <- T2M / delta_T
  diffusion <- NULL
  
  diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(delta_T)), ncol = nsims, nrow = periods)
  S <- exp((drift - (sigma^2) / 2) * delta_T + sigma * diffusion)
  S <- apply(rbind(rep(spot,nsims),S), 2, cumprod)
  S <- dplyr::as_tibble(S, .name_repair = "minimal")
  names(S) <- paste("sim", 1:nsims, sep = "")
  S <- S %>% dplyr::mutate(t = seq(0, T2M, delta_T)) %>% dplyr::select(t, dplyr::everything())
  
  return(S)
}

