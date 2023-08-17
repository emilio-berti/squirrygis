#' @title BIO02 Mean Annual Diurnal Range
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' @return one raster with the bioclimatic variable.
bio02 <- function(tmax, tmin) {
  stopifnot(is(tmax, "SpatRaster"))
  stopifnot(is(tmin, "SpatRaster"))
  stopifnot(nlyr(tmax) == 12)
  stopifnot(nlyr(tmin) == 12)
  if (! identical(names(tmax), as.character(1:12)) ) {
    warning("tmax stack doesn't have valid names or they are not in the correct order")
  }
  if (! identical(names(tmin), as.character(1:12)) ) {
    warning("tmin stack doesn't have valid names or they are not in the correct order")
  }
  r <- tmax - tmin
  r <- mean(r)
  names(r) <- "BIO02"
  return (r)
}
