#' @title BIO06 Minimum Temperature Coldest Month
#' @param tmin raster stack of monthly average temperatures.
#' @return one raster with the bioclimatic variable.
bio06 <- function(tmin) {
  stopifnot(is(tmin, "RasterStack"))
  stopifnot(nlayers(tmin) == 12)
  if (! identical(names(tmin), paste0("X", as.character(1:12))) ) {
    warning("tmin doesn't have valid names or they are not in the correct order")
  }
  r <- min(tmin)
  names(r) <- "BIO06"
  return (r)
}
