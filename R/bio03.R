#' @title BIO03 Annual Mean Isothermality
#' @param tas raster stack of monthly average temperatures.
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' 
#' @details In contrast to CHELSA, this is not multiplied by 100
#' 
#' @return one raster with the bioclimatic variable.
bio03 <- function(tas, tmax, tmin) {
  stopifnot(is(tas, "SpatRaster"))
  stopifnot(is(tmax, "SpatRaster"))
  stopifnot(is(tmin, "SpatRaster"))
  stopifnot(nlyr(tas) == 12)
  stopifnot(nlyr(tmax) == 12)
  stopifnot(nlyr(tmin) == 12)
  if (! identical(names(tas), as.character(1:12)) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  if (! identical(names(tmax), as.character(1:12)) ) {
    warning("tmax doesn't have valid names or they are not in the correct order")
  }
  if (! identical(names(tmin), as.character(1:12)) ) {
    warning("tmin doesn't have valid names or they are not in the correct order")
  }
  r <- tas * (1 - tmin / tmax) - tmin
  r <- mean(r)
  names(r) <- "BIO03"
  return (r)
}
