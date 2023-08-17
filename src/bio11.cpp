#include "Rcpp.h"

using namespace Rcpp;

//' @name bio11
//' @title Mean Temperature of Coldest Quarter
//' @param tas matrix of average temperature timeseries.
//' @return matrix with average temperature of the coldest quarter (first
//'   column) and the first month of the quarter (second column).
// [[Rcpp::export]]
NumericMatrix cpp_bio11( NumericMatrix tas ) {
  int rows = tas.nrow();
  int cols = tas.ncol();
  double window, val;
  NumericMatrix ans( rows , 2 );
  
  if (cols != 12) {
    return NumericMatrix(1, 1);
  }
  
  for ( int i = 0 ; i < rows; i++ ) {
    val = 10000000;
    for ( int j = 0; j < (cols - 2); j++ ) {
      window = tas(i, j) + tas(i, j + 1) + tas(i, j + 2);
      if ( j == 0 or window < val ) {
        val = window;
        ans(i, 0) = (tas(i, j) + tas(i, j + 1) + tas(i, j + 2)) / 3;
        ans(i, 1) = j + 1; //first month of the quarter
      }
    }
  }
  
  return ans;
}
