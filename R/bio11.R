#' @title BIO11 Mean Temperature of Coldest Quarter
#' @param tas raster stack of monthly average temperatures.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio11 <- function(tas, also.quarter = FALSE) {
  stopifnot(is(tas, "SpatRaster"))
  stopifnot(nlyr(tas) == 12)
  if (! identical(names(tas), as.character(1:12)) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  
  t <- matrix(tas, ncol = 12)
  
  r <- cpp_bio11(t)
  
  q <- r[, 2]
  r <- r[, 1]
  q <- matrix(q, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  r <- matrix(r, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  
  r <- rast(r)
  q <- rast(q)
  r <- c(r, q)
  crs(r) <- crs(tas)
  names(r) <- c("BIO11", "start.quarter")
  
  if (also.quarter == FALSE) r <- r[["BIO11"]]
  
  return (r)
}
