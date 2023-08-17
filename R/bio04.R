#' @title BIO04 Mean Annual Temperature Seasonality
#' @param tas raster stack of monthly average temperatures.
#' 
#' @details Standard deviation of the monthly mean temperatures.
#' In contrast to CHELSA, this is not multiplied by 100
#' 
#' @return one raster with the bioclimatic variable.
bio04 <- function(tas) {
  stopifnot(is(tas, "SpatRaster"))
  stopifnot(nlyr(tas) == 12)
  if (! identical(names(tas), as.character(1:12)) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  mu <- mean(tas)
  r <- (tas - mu) ^ 2
  r <- 1 / 11 * sum(r) #unbiased variance
  r <- sqrt(r)
  names(r) <- "BIO04"
  return (r)
}
