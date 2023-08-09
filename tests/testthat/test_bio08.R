test_that("BIO08 c++", {
  set.seed(1234)
  require(terra)
  
  tas <- c(
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5)),
    rast(matrix(rnorm(25), 5, 5))
  )
  names(tas) <- as.character(1:12)
  
  pr <- c(
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5)),
    rast(matrix(runif(25, 5, 25), 5, 5))
  )
  names(pr) <- as.character(1:12)
  
  cpp <- bio08(tas, pr, also.quarter = TRUE)
  
  q <- matrix(NA, nrow(tas), ncol(tas))
  r <- matrix(NA, nrow(tas), ncol(tas))
  for (i in seq_len(nrow(tas))) {
    for (j in seq_len(ncol(tas))) {
      val <- 0
      for (k in seq(1, 10)) {
        window <- sum(pr[i, j, (k : (k + 2))])
        if (k == 1 || window > val) {
          val <- window
          q[i, j] <- k
        }
      }
      r[i, j] <- mean(as.numeric(tas[i, j, (q[i, j] : (q[i, j] + 2))]))
    }
  }
  baseR <- c(rast(r), rast(q))
  names(baseR) <- c("BIO08", "start.quarter")

  expect_equal(
    values(baseR), values(cpp)
  )
})
