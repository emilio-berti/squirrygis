#' @title BIO03 Mean Annual Isothermality
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' 
#' @details In contrast to CHELSA, this is not multiplied by 100
#' 
#' @return one raster with the bioclimatic variable.
bio03 <- function(tmax, tmin) {
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
  num <- tmax - tmin
  num <- mean(num)
  denom <- max(tmax) - min(tmin)
  r <- num / denom
  names(r) <- "BIO03"
  return (r)
}
