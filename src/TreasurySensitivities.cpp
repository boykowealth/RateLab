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
NumericMatrix TreasurySensitivities(NumericMatrix x, double step) {
  
  NumericMatrix results(x.nrow(), 9);
  
  for (int i = 0; i < x.nrow(); i++) {
    
    double index = x(i, 0);
    double rate = x(i, 1);
    double coupon = x(i, 2);
    double t2m = x(i, 3);
    double price = x(i, 4);
    double price_plus = x(i, 5);
    double price_minus = x(i, 6);
    
    double delta = (price_plus - price_minus) / (2 * step) / 10000;
    double gamma = 0.5 * ((price_plus - 2 * price + price_minus) / pow(step, 2)) / pow(10000, 2);
    
    results(i, 0) = index;
    results(i, 1) = rate;
    results(i, 2) = coupon;
    results(i, 3) = t2m;
    results(i, 4) = price;
    results(i, 5) = price_plus;
    results(i, 6) = price_minus;
    results(i, 7) = delta;
    results(i, 8) = gamma;
    
  }
  
  colnames(results) = CharacterVector::create("Index", "Rate","Coupon", "t2m", "Price", "Price_Plus", "Price_Minus", "Delta", "Gamma");
  
  return results;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
