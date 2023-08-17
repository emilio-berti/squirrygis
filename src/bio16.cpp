#include "Rcpp.h"

using namespace Rcpp;

//' @name bio16
//' @title Mean Precipitation of Wettest Quarter
//' @param pr matrix of precipitation timeseries.
//' @return matrix with average precipitation of the wettest quarter (first
//'   column) and the first month of the quarter (second column).
// [[Rcpp::export]]
NumericMatrix cpp_bio16( NumericMatrix pr ) {
  int rows = pr.nrow();
  int cols = pr.ncol();
  double window, val;
  NumericMatrix ans( rows , 2 );
  
  if (cols != 12) {
    return NumericMatrix(1, 1);
  }
  
  for ( int i = 0 ; i < rows; i++ ) {
    val = 0;
    for ( int j = 0; j < (cols - 2); j++ ) {
      window = pr(i, j) + pr(i, j + 1) + pr(i, j + 2);
      if ( j == 0 or window > val ) {
        val = window;
        ans(i, 0) = (pr(i, j) + pr(i, j + 1) + pr(i, j + 2)) / 3;
        ans(i, 1) = j + 1; //first month of the quarter
      }
    }
  }
  
  return ans;
}
