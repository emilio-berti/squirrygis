#' @title BIO03 Mean Annual Isothermality
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' 
#' @details In contrast to CHELSA, this is not multiplied by 100
#' 
#' @return one raster with the bioclimatic variable.
bio03 <- function(tmax, tmin) {
  stopifnot(is(tmax, "SpatRaster"))
  stopifnot(is(tmin, "SpatRaster"))
  stopifnot(nlyr(tmax) == 12)
  stopifnot(nlyr(tmin) == 12)
  if (! identical(names(tmax), as.character(1:12)) ) {
    warning("tmax doesn't have valid names or they are not in the correct order")
  }
  if (! identical(names(tmin), as.character(1:12)) ) {
    warning("tmin doesn't have valid names or they are not in the correct order")
  }
  num <- tmax - tmin
  num <- mean(num)
  denom <- max(tmax) - min(tmin)
  r <- num / denom
  names(r) <- "BIO03"
  return (r)
}
