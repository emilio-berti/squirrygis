#' @title BIO06 Annual Mean Maximum Temperature
#' @param tmin raster stack of monthly average temperatures.
#' @return one raster with the bioclimatic variable.
bio06 <- function(tmin) {
  stopifnot(is(tmin, "SpatRaster"))
  stopifnot(nlyr(tmin) == 12)
  if (! identical(names(tmin), as.character(1:12)) ) {
    warning("tmin doesn't have valid names or they are not in the correct order")
  }
  r <- min(tmin)
  names(r) <- "BIO06"
  return (r)
}
