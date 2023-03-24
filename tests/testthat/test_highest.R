test_that("Lowest Quarter", {
  a <- matrix(c(
    1:12, 
    12:1,
    c(1:6, 6:1),
    c(6:1, 1:6)
  ), byrow = TRUE, ncol = 12)
  correct <- rbind(
    c(11, 10),
    c(11, 1),
    c(17/3, 5),
    c(15/3, 1)
  )
  ans <- highest_quarter(a)
  expect_equal(correct, ans, tolerance = 1e-6)
})
