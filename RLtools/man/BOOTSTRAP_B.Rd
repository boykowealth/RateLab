% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BOOTSTRAP_B.R
\name{BOOTSTRAP_B}
\alias{BOOTSTRAP_B}
\title{Bootstrap Yield Curve from Bond Prices and Coupons}
\usage{
BOOTSTRAP_B(bond_data)
}
\arguments{
\item{data}{A data frame with bond prices, coupon rates, and maturities.}
}
\value{
A tibble with maturities and zero rates.
}
\description{
This function bootstraps a zero-coupon yield curve from bond prices and coupon rates.
It assumes annual coupons and that the bond prices are known.
}
\examples{
bonds <- data.frame(
          Maturity = c(1, 2, 3),
          Price = c(950, 920, 880),
          Coupon = c(0, 50, 60),
          FaceValue = 1000
          )
BOOTSTRAP_B(bond_data = bond_data)
}
