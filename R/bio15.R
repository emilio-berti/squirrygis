#' @title BIO15 Precipitation Seasonality
#' @param pr raster stack of monthly precipitation.
#' 
#' @details
#' Seasonality is defined as the coefficient of variation, 
#' i.e. sd(pr) / divided by mean(pr)
#' 
#' @return one raster with the bioclimatic variable.
bio14 <- function(pr) {
  stopifnot(is(pr, "SpatRaster"))
  stopifnot(nlyr(pr) == 12)
  if (! identical(names(pr), as.character(1:12)) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  
  mu <- mean(pr)
  r <- (pr - mu) ^ 2
  r <- 1 / 11 * sum(r) #unbiased variance
  r <- sqrt(r) / mu
  names(r) <- "BIO15"
  
  return (r)
}
