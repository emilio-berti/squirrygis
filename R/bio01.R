#' @title BIO01 Annual Mean Temperature
#' @param tas raster stack of monthly average temperatures.
#' @return one raster with the bioclimatic variable.
bio01 <- function(tas) {
  stopifnot(is(tas, "SpatRaster"))
  stopifnot(nlyr(tas) == 12)
  if (! identical(names(tas), as.character(1:12)) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  r <- mean(tas)
  names(r) <- "BIO01"
  return (r)
}
