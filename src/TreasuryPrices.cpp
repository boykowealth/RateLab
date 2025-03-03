#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix TreasuryPrices(NumericMatrix x, double step) {
  
  NumericMatrix results(x.nrow(), 7);
  
    for (int i = 0; i < x.nrow(); i++) {
      
    double index = x(i, 0);
    double ytm = x(i, 1);
    double face = 100; //changed to 100 to standardize price
    double m = 2;
    int t2m = x(i, 3);
    double C = x(i, 2);
    
    if (t2m > 1) {
      m = 2;
    } else {
      m = 1;
    }
    
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
    
    results(i, 0) = index;
    results(i, 1) = ytm;
    results(i, 2) = C;
    results(i, 3) = t2m;
    results(i, 4) = price;
    results(i, 5) = priceplus;
    results(i, 6) = priceminus;
    
    }
    
  colnames(results) = CharacterVector::create("Index", "Rate", "Coupon", "t2m", "Price", "Price_Plus", "Price_Minus");
  return results;
    
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
dat <- TREASURY_US() %>% 
  dplyr::mutate(n = dplyr::row_number())

datesmat <- dat %>% select(n, Date, Maturity)

params <- dat %>% select(n, c(-Date, -Maturity))

matrix <- params %>% as.matrix()

TreasuryPrices(matrix, 0.0001)
*/
