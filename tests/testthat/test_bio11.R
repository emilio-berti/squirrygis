test_that("BIO11 c++", {
  set.seed(1234)
  require(raster)
  
  tas <- stack(
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5)),
    raster(matrix(rnorm(25), 5, 5))
  )
  names(tas) <- as.character(1:12)
  
  cpp <- bio11(tas, also.quarter = TRUE)
  
  q <- matrix(NA, nrow(tas), ncol(tas))
  r <- matrix(NA, nrow(tas), ncol(tas))
  for (i in seq_len(nrow(tas))) {
    for (j in seq_len(ncol(tas))) {
      val <- Inf
      for (k in seq(1, 10)) {
        window <- sum(tas[i, j][(k : (k + 2))])
        if (k == 1 || window < val) {
          val <- window
          q[i, j] <- k
        }
      }
      r[i, j] <- mean(as.numeric(tas[i, j][(q[i, j] : (q[i, j] + 2))]))
    }
  }
  baseR <- stack(raster(r), raster(q))
  names(baseR) <- c("BIO11", "start.quarter")
  
  expect_equal(
    values(baseR), values(cpp)
  )
})
