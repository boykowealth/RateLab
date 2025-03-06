#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix CrossGamma(NumericMatrix x, NumericMatrix y, double step) {
  
  int n = x.nrow();
  NumericMatrix result(n,n, NA_REAL);
  
  for (int i = 0; i < n; i++){
    for (int j = 0; j < n; j++){
      if (i == j){
        
        result(i,j) = x(i, 5);
        
      }//else{
        
        //result(i,j) = NULL;
      //}
    }
  }
  return result;
}





