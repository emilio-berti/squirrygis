#' @title BIO17 Mean Precipitation of Driest Quarter
#' @param pr raster stack of monthly precipitation.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio17 <- function(pr, also.quarter = FALSE) {
  stopifnot(is(pr, "RasterStack"))
  stopifnot(nlayers(pr) == 12)
  if (! identical(names(pr), paste0("X", as.character(1:12))) ) {
    warning("pr doesn't have valid names or they are not in the correct order")
  }
  
  t <- matrix(pr, ncol = 12)
  
  r <- cpp_bio17(t)
  
  q <- r[, 2]
  r <- r[, 1]
  q <- matrix(q, nrow = nrow(pr), ncol = ncol(pr), byrow = TRUE)
  r <- matrix(r, nrow = nrow(pr), ncol = ncol(pr), byrow = TRUE)
  
  r <- raster(r)
  q <- raster(q)
  r <- stack(r, q)
  crs(r) <- crs(pr)
  extent(r) <- extent(pr)
  names(r) <- c("BIO17", "start.quarter")
  
  if (also.quarter == FALSE) r <- r[["BIO17"]]
  
  return (r)
}
