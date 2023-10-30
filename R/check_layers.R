#' @title Check temperature layers
#' @param t timeseries of temperatures.
#' @param K0 absolute zero in Celsius.
#' @return NUll
check_temperature <- function(t, K0 = -273.15) {
  if ( any(names(t) != paste0("X", as.character(1:12))) ) {
    warning ("Layer names should be 'X1', 'X2', ..., 'X12'.")
  }
  
  if ( any(minValue(t) < K0) ) {
    warning ("Temperatures below 0 Kelvin.")
  }
  
  if ( any(maxValue(t) > 60 - K0) ) {
    warning ("Temperatures above 60 Celsius.")
  }
  
  if ( any(minValue(t) < 0) ) {
    warning ("Negative temperatures: Kelvin = Celsius +", -K0, ".")
  }
}

#' @title Check precipitation layers
#' @param p timeseries of precipitations.
#' @return NUll
check_precipitation <- function(p) {
  if ( any(names(p) != paste0("X", as.character(1:12))) ) {
    warning ("Layer names should be 'X1', 'X2', ..., 'X12'.")
  }
  
  if ( any(minValue(p) < 0) ) {
    warning ("Precipitations below 0.")
  }
}
