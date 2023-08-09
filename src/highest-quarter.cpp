#include "Rcpp.h"

using namespace Rcpp;

//' @name highest_quarter
//' @title Average Value of the Highest Quarter
//' @param x matrix of timeseries.
//' @return matrix with average climate of the highest quarter (first
//'   column) and the first month of the quarter (second column).
// [[Rcpp::export]]
NumericMatrix highest_quarter( NumericMatrix x ) {
  int rows = x.nrow();
  int cols = x.ncol();
  double window, val;
  NumericMatrix ans( rows , 2 );
  
  if (cols != 12) {
    return NumericMatrix(1, 1);
  }
  
  for ( int i = 0 ; i < rows; i++ ) {
    for ( int j = 0; j < (cols - 2); j++ ) {
      window = x(i, j) + x(i, j + 1) + x(i, j + 2);
      window = window / 3;
      if ( j == 0 or window > val ) {
        val = window;
        ans(i, 1) = j + 1; //first month of the quarter
      }
    }
    ans(i, 0) = val; //value of the quarter
  }
  
  return ans;
}
