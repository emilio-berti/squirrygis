#' @title BIO18 Mean Precipitation of Warmest Quarter
#' @param tas raster stack of monthly average temperatures.
#' @param pr raster stack of monthly precipitation.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio18 <- function(tas, pr, also.quarter = FALSE) {
  stopifnot(is(tas, "RasterStack"))
  stopifnot(nlayers(tas) == 12)
  if (! identical(names(tas), paste0("X", as.character(1:12))) ) {
    warning("tas stack doesn't have valid names or they are not in the correct order")
  }
  stopifnot(is(pr, "RasterStack"))
  stopifnot(nlayers(pr) == 12)
  if (! identical(names(pr), paste0("X", as.character(1:12))) ) {
    warning("pr stack doesn't have valid names or they are not in the correct order")
  }

  t <- matrix(tas, ncol = 12)
  p <- matrix(pr, ncol = 12)
  
  r <- cpp_bio18(t, p)
  
  q <- r[, 2]
  r <- r[, 1]
  q <- matrix(q, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  r <- matrix(r, nrow = nrow(tas), ncol = ncol(tas), byrow = TRUE)
  
  r <- raster(r)
  q <- raster(q)
  r <- stack(r, q)
  crs(r) <- crs(tas)
  extent(r) <- extent(tas)
  names(r) <- c("BIO18", "start.quarter")
  
  if (also.quarter == FALSE) r <- r[["BIO18"]]
  
  return (r)
}
