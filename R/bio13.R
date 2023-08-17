#' @title BIO13 Precipitation of Wettest Month
#' @param pr raster stack of monthly precipitation.
#' @return one raster with the bioclimatic variable.
bio13 <- function(pr) {
  stopifnot(is(pr, "SpatRaster"))
  stopifnot(nlyr(pr) == 12)
  if (! identical(names(pr), as.character(1:12)) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  
  r <- max(pr)
  names(r) <- "BIO13"
  
  return (r)
}
