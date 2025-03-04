#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector Sensitivities(NumericMatrix x, double step) {
  
  NumericMatrix results(x.nrow(), 4);
  
  for (int i = 0; i < x.nrow(); i++) {
    
    double ytm = x(i, 0);
    double t2m = x(i, 2);
    double price = x(i, 3);
    double price_plus = x(i, 4);
    double price_minus = x(i, 5);
    
    double delta = (price_plus - price_minus) / (2 * step) / 10000;
    double gamma = 0.5 * ((price_plus - 2 * price + price_minus) / pow(step, 2)) / pow(10000, 2);
    
    results(i, 0) = ytm;
    results(i, 1) = t2m;
    results(i, 2) = delta;
    results(i, 3) = gamma;
     
  }
  
  colnames(results) = CharacterVector::create("YTM", "T2M","Delta", "Gamma");
  return results;
}




