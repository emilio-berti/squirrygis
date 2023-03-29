test_that("bio10", {
  set.seed(1234)
  require(terra)
  r <- c(
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE)),
    rast(matrix(round(runif(4, 1, 10)), ncol = 2, byrow = TRUE))
  )
  names(r) <- 1:12
  
  values(r)
  correct <- rbind(
    c(20/3, 2),
    c(22/3, 2),
    c(21/3, 10),
    c(23/3, 9)
  )
  ans <- bio10(r, also.quarter = TRUE)
  ans <- values(ans)
  dimnames(ans) <- NULL
  
  expect_equal(correct, ans, tolerance = 1e-6)
})
