#' @title BIO10 Mean Temperature of Warmest Quarter
#' @param tas raster stack of monthly average temperatures.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio10 <- function(tas, also.quarter = FALSE) {
  stopifnot(is(tas, "RasterStack"))
  stopifnot(nlayers(tas) == 12)
  if (! identical(names(tas), paste0("X", as.character(1:12))) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  
  t <- matrix(tas, ncol = 12)
  
  r <- cpp_bio10(t)
  
  q <- r[, 2]
  r <- r[, 1]
  q <- matrix(q, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  r <- matrix(r, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  
  r <- raster(r)
  q <- raster(q)
  r <- stack(r, q)
  crs(r) <- crs(tas)
  extent(r) <- extent(tas)
  names(r) <- c("BIO10", "start.quarter")
  
  if (also.quarter == FALSE) r <- r[["BIO10"]]
  
  return (r)
}
