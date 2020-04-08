# ------------------------------------------------------------------------------
# lubridate

test_that("lubridate::tz() returns UTC", {
  expect_identical(lubridate::tz(new_ym(1L)), "UTC")
})

test_that("lubridate accessors work", {
  x <- new_ym(0L)

  expect_identical(lubridate::year(x), 1970L)
  expect_identical(lubridate::month(x), 1L)
  expect_identical(lubridate::day(x), 1L)
  expect_identical(lubridate::hour(x), 0L)
  expect_identical(lubridate::minute(x), 0L)
  expect_identical(lubridate::second(x), 0)
})
