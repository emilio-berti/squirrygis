test_that("BIO16 c++", {
  set.seed(1234)
  require(terra)
  
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
  
  cpp <- bio16(pr, also.quarter = TRUE)
  
  q <- matrix(NA, nrow(pr), ncol(pr))
  r <- matrix(NA, nrow(pr), ncol(pr))
  for (i in seq_len(nrow(pr))) {
    for (j in seq_len(ncol(pr))) {
      val <- 0
      for (k in seq(1, 10)) {
        window <- sum(pr[i, j, (k : (k + 2))])
        if (k == 1 || window > val) {
          val <- window
          q[i, j] <- k
        }
      }
      r[i, j] <- mean(as.numeric(pr[i, j, (q[i, j] : (q[i, j] + 2))]))
    }
  }
  baseR <- c(rast(r), rast(q))
  names(baseR) <- c("BIO16", "start.quarter")
  
  expect_equal(
    values(baseR), values(cpp)
  )
})
