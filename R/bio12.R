#' @title BIO12 Total Annual Precipitation
#' @param pr raster stack of monthly precipitation.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio12 <- function(pr, also.quarter = FALSE) {
  stopifnot(is(pr, "SpatRaster"))
  stopifnot(nlyr(pr) == 12)
  if (! identical(names(pr), as.character(1:12)) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  
  r <- sum(pr)
  names(r) <- "BIO12"
  
  return (r)
}
