#include <Rcpp.h>
using namespace Rcpp;

//

// [[Rcpp::export]]
NumericMatrix CrossDelta(NumericMatrix x, NumericMatrix y) {
  
  int n = x.nrow();
  NumericMatrix result(n,n);
  
  for (int i = 0; i < n; i++){
    
    double price = x(i,2);
    
    for (int j = 0; j < n; j++){
      
      double yield = y(j, 0);
      double change = y(j, 1);
        
      if (i == j){
        
        result(i,j) = x(i, 3);
        
      }else{
      
      result(i,j) = (price * (yield + change) - price * (yield - change)) / (2 * change) / 10000;
        
      }
    }
  }
  return result;
}





