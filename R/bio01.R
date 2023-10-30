#' @title BIO01 Mean Annual Temperature
#' @param tas raster stack of monthly average temperatures.
#' @return one raster with the bioclimatic variable.
bio01 <- function(tas) {
  stopifnot(is(tas, "RasterStack"))
  stopifnot(nlayers(tas) == 12)
  if (! identical(names(tas), paste0("X", as.character(1:12))) ) {
    warning("tas doesn't have valid names or they are not in the correct order")
  }
  r <- mean(tas)
  names(r) <- "BIO01"
  return (r)
}
