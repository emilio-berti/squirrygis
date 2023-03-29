#' @title BIO10 Mean Temperature of the Warmest Quarter
#' @param s raster stack of monthly temperatures.
#' @param also.quarter TRUE/FALSE (default FALSE), if to return also the 
#'  starting month of the warmest quarter.
#' @return one raster with the bioclimatic variable.
bio10 <- function(s, also.quarter = FALSE) {
  stopifnot(is(s, "SpatRaster"))
  stopifnot(nlyr(s) == 12)
  if (! identical(names(s), as.character(1:12)) ) {
    warning("Raster stack doesn't have valid names or they are not in the correct order")
  }
  
  x <- values(s)
  x <- as.matrix(x)
  ans <- highest_quarter(x)
  
  r <- s[[1]]
  r[!is.na(r)] <- -9999
  r[!is.na(r)] <- ans[, 1]
  
  if (also.quarter == TRUE) {
    q <- s[[1]]
    q[!is.na(q)] <- -9999
    q[!is.na(q)] <- ans[, 2]
    r <- c(r, q)
    names(r) <- c("BIO10", "Starting.quarter")
  } else {
    names(r) <- "BIO10"
  }
  
  return (r)
}
