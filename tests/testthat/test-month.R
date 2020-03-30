test_that("results are correct by brute force comparison", {
  # -2300-01-01 -> 2300-01-01
  x <- months_to_days(-51240:3960)

  from <- structure(-1559585, class = "Date")
  to <- as.Date("2300-01-01")

  expect <- seq.Date(from, to, by = "1 month")
  expect <- unclass(expect)
  expect <- as.integer(expect)

  expect_identical(x, expect)
})
