#' @title Check temperature layers
#' @param t timeseries of temperatures.
#' @param K0 absolute zero in Celsius.
#' @return NUll
check_temperature <- function(t, K0 = -273.15) {
  if ( any(names(t) != as.character(1:12)) ) {
    warning ("Layer names should be '1', '2', ..., '12'.")
  }
  
  if ( any(t@ptr$range_min < K0) ) {
    warning ("Temperatures below 0 Kelvin.")
  }
  
  if ( any(t@ptr$range_min > 60 - K0) ) {
    warning ("Temperatures above 60 Celsius.")
  }
  
  if ( any(t@ptr$range_min < 0) ) {
    warning ("Negative temperatures: Kelvin = Celsius +", -K0, ".")
  }
}

#' @title Check precipitation layers
#' @param p timeseries of precipitations.
#' @return NUll
check_precipitation <- function(p) {
  if ( any(names(p) != as.character(1:12)) ) {
    warning ("Layer names should be '1', '2', ..., '12'.")
  }
  
  if ( any(p@ptr$range_min < 0) ) {
    warning ("Precipitations below 0.")
  }
}
