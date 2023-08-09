#' @title BIO05 Annual Mean Maximum Temperature
#' @param tmax raster stack of monthly average temperatures.
#' @return one raster with the bioclimatic variable.
bio05 <- function(tmax) {
  stopifnot(is(tmax, "SpatRaster"))
  stopifnot(nlyr(tmax) == 12)
  if (! identical(names(tmax), as.character(1:12)) ) {
    warning("tmax doesn't have valid names or they are not in the correct order")
  }
  r <- max(tmax)
  names(r) <- "BIO05"
  return (r)
}
