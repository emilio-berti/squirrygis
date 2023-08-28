#' @title BIO09 Mean Temperature of Driest Quarter
#' @param tas raster stack of monthly average temperatures.
#' @param pr raster stack of monthly precipitation.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio09 <- function(tas, pr, also.quarter = FALSE) {
  stopifnot(is(tas, "SpatRaster"))
  stopifnot(is(pr, "SpatRaster"))
  stopifnot(nlyr(tas) == 12)
  stopifnot(nlyr(pr) == 12)
  if (! identical(names(tas), as.character(1:12)) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  if (! identical(names(pr), as.character(1:12)) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  stopifnot(nrow(tas) == nrow(pr))
  
  t <- matrix(tas, ncol = 12)
  p <- matrix(pr, ncol = 12)
  
  r <- cpp_bio09(t, p)
  
  q <- r[, 2]
  r <- r[, 1]
  q <- matrix(q, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  r <- matrix(r, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  
  r <- rast(r)
  q <- rast(q)
  r <- c(r, q)
  crs(r) <- crs(tas)
  ext(r) <- ext(tas)
  names(r) <- c("BIO09", "start.quarter")
  
  if (also.quarter == FALSE) r <- r[["BIO09"]]
  
  return (r)
}
