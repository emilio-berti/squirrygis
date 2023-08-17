#' @title BIO14 Precipitation of Driest Month
#' @param pr raster stack of monthly precipitation.
#' @return one raster with the bioclimatic variable.
bio14 <- function(pr) {
  stopifnot(is(pr, "SpatRaster"))
  stopifnot(nlyr(pr) == 12)
  if (! identical(names(pr), as.character(1:12)) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  
  r <- min(pr)
  names(r) <- "BIO14"
  
  return (r)
}
