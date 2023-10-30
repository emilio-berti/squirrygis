#' @title Bioclimatic Variables
#' @param tas raster stack of monthly average temperatures.
#' @param tmax raster stack of monthly maximum temperatures.
#' @param tmin raster stack of monthly minimum temperatures.
#' @param pr raster stack of monthly precipitation.
#' @param checks boolean, to run some checks on input layers.
#' 
#' @details This calculates all bioclimatic variables.
#' 
#' @return a raster stack.
bioclim <- function(tas, tmin, tmax, pr, checks = FALSE) {
  if (checks) {
    check_temperature(tas)
    check_temperature(tmin)
    check_temperature(tmax)
  }
  b1 <- bio01(tas)
  b2 <- bio02(tmax, tmin)
  b3 <- bio03(tmax, tmin)
  b4 <- bio04(tas)
  b5 <- bio05(tmax)
  b6 <- bio06(tmin)
  b7 <- bio07(tmax, tmin)
  b8 <- bio08(tas, pr)
  b9 <- bio09(tas, pr)
  b10 <- bio10(tas)
  b11 <- bio11(tas)
  b12 <- bio12(pr)
  b13 <- bio13(pr)
  b14 <- bio14(pr)
  b15 <- bio15(pr)
  b16 <- bio16(pr)
  b17 <- bio17(pr)
  b18 <- bio18(tas, pr)
  b19 <- bio19(tas, pr)
  ans <- stack(
    b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,
    b12, b13, b14, b15, b16, b17, b18, b19
  )
  # names(ans) <- paste0("BIO", seq_len(19))
  return (ans)
}