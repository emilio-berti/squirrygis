#' @title BIO02 Mean Annual Diurnal Range
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' @return one raster with the bioclimatic variable.
bio02 <- function(tmax, tmin) {
  stopifnot(is(tmax, "RasterStack"))
  stopifnot(nlayers(tmax) == 12)
  if (! identical(names(tmax), paste0("X", as.character(1:12))) ) {
    warning("tmax stack doesn't have valid names or they are not in the correct order")
  }
  stopifnot(is(tmin, "RasterStack"))
  stopifnot(nlayers(tmin) == 12)
  if (! identical(names(tmin), paste0("X", as.character(1:12))) ) {
    warning("tmin stack doesn't have valid names or they are not in the correct order")
  }
  r <- tmax - tmin
  r <- mean(r)
  names(r) <- "BIO02"
  return (r)
}
