#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix bond_prices(NumericMatrix x, double step) {
  
  NumericMatrix results(x.nrow(), 6);
  
  for (int i = 0; i < x.nrow(); i++) {
    
    double ytm = x(i, 0);
    double face = x(i, 1); //change to 100 to standardize price
    double m = x(i,4);
    int t2m = x(i, 2);
    double C = x(i, 3);

    double price = 0.00;
    double priceplus = 0.00;
    double priceminus = 0.00;
    
    
    for (double t = 1.0/m; t <= t2m; t+= 1.0/m){
      
      double t_periods = m * t;

      double discount_factor = 1.0 / pow(1.0 + ytm / m, t_periods);
      
      double discount_facor_plus = 1.0 / pow(1.0 + (ytm + step) / m, t_periods);
      
      double discount_facor_minus = 1.0 / pow(1.0 + (ytm - step) / m, t_periods);

      double cf;

      if (t == t2m){
        cf =  C * face / m + face;
      } else{
        cf = C * face / m;
      }

      double pv = cf * discount_factor;
      double pvp = cf * discount_facor_plus;
      double pvm = cf * discount_facor_minus;

      price += pv;
      priceplus += pvp;
      priceminus += pvm;

    }
    
    results(i, 0) = ytm;
    results(i, 1) = face;
    results(i, 2) = t2m;
    results(i, 3) = price;
    results(i, 4) = priceplus;
    results(i, 5) = priceminus;
    
    
  }
  
  colnames(results) = CharacterVector::create("YTM", "FaceValue", "T2M", "Price", "Price_Plus", "Price_Minus");
  return results;
}


/*** R
library(tidyverse)


l10bond <- c(1000000, 10, 0.03, 0.03, 2)
s2bond <- c(500000, 2, 0.03, 0.03, 2)
s30bond <- c(500000, 30, 0.03, 0.03, 2)

ytms <- round(seq(0.00, 0.06,0.0001), 4)


params <- tibble::tibble(
  face = c(l10bond[[1]], s2bond[[1]], s30bond[[1]]),
  t2m = c(l10bond[[2]], s2bond[[2]], s30bond[[2]]),
  C = c(l10bond[[3]], s2bond[[3]], s30bond[[3]]),
  m = c(l10bond[[5]], s2bond[[5]], s30bond[[5]])
) %>%
  dplyr::mutate(nbond = row_number())


data <- expand.grid(bond = 1:nrow(params), ytm = ytms) %>%
  dplyr::left_join(params, by = c('bond' = 'nbond')) %>%
  dplyr::select(-bond)

data <- data %>% as.matrix()

bond_prices(data, 0.001)
*/
