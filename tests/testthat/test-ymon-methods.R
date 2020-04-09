# ------------------------------------------------------------------------------
# Base

test_that("as.list() works", {
  x <- set_names(new_ymon(1:2), c("x", "y"))

  expect <- list(
    set_names(new_ymon(1L), "x"),
    set_names(new_ymon(2L), "y")
  )

  expect_identical(as.list(x), expect)
})

test_that("c() works as best as it can", {
  expect_identical(c(new_ymon(1L), new_ymon(2L)), new_ymon(1:2))

  # Nothing we can do about the second case
  expect_error(c(new_ymon(1L), 1), class = "vctrs_error_incompatible_type")
  expect_identical(c(1, new_ymon(1L)), c(1, 1))
})

test_that("`[` works", {
  expect_identical(new_ymon(1:2)[], new_ymon(1:2))
  expect_identical(new_ymon(1:2)[1], new_ymon(1L))

  x <- set_names(new_ymon(1:2), c("x", "y"))
  expect_identical(x["y"], set_names(new_ymon(2L), "y"))
})

test_that("`[` has nice error message with extra indices", {
  expect_error(new_ymon(1L)[1, 1], class = "rlib_error_dots_nonempty")
})

test_that("`[[` works", {
  x <- new_ymon(1:2)
  expect_identical(x[[1]], new_ymon(1L))

  # names are dropped
  x <- set_names(x, c("x", "y"))
  expect_identical(x[["y"]], new_ymon(2L))
})

test_that("weekdays() works", {
  expect_identical(weekdays(new_ymon(c(NA_integer_, 0L))), c(NA, "Thursday"))
  expect_identical(weekdays(new_ymon(0L), abbreviate = TRUE), "Thu")
})

test_that("months() works", {
  expect_identical(months(new_ymon(c(NA_integer_, 0L))), c(NA, "January"))
  expect_identical(months(new_ymon(0L), abbreviate = TRUE), "Jan")
})

test_that("quarters() works", {
  expect_identical(quarters(new_ymon(c(NA_integer_, 0L))), c(NA, "Q1"))

  expect_identical(
    quarters(new_ymon(0:11)),
    rep(c("Q1", "Q2", "Q3", "Q4"), each = 3)
  )
})

test_that("julian() works", {
  x <- new_ymon(c(NA_integer_, 0L, 1L))
  expect <- structure(c(NA, 0L, 31L), origin = ymon(1970, 1))
  expect_identical(julian(x), expect)

  origin <- ymon(1970, 2)
  expect <- structure(c(NA, -31L, 0L), origin = origin)
  expect_identical(julian(x, origin = origin), expect)

  expect_error(julian(new_ymon(), origin = 1), "length 1 ymon")
  expect_error(julian(new_ymon(), origin = new_ymon(1:2)), "length 1 ymon")
  expect_error(julian(new_ymon(), origin = new_ymon(NA_integer_)), "length 1 ymon")

  expect_error(julian(new_ymon(), origin = new_ymon(1L), y = 1), class = "rlib_error_dots_nonempty")
})

# ------------------------------------------------------------------------------
# lubridate

test_that("lubridate::tz() returns UTC", {
  expect_identical(lubridate::tz(new_ymon(1L)), "UTC")
})

test_that("lubridate accessors work", {
  x <- new_ymon(0L)

  expect_identical(lubridate::year(x), 1970)
  expect_identical(lubridate::month(x), 1)
  expect_identical(lubridate::day(x), 1L)
  expect_identical(lubridate::hour(x), 0L)
  expect_identical(lubridate::minute(x), 0L)
  expect_identical(lubridate::second(x), 0)
})

# ------------------------------------------------------------------------------
# vctrs

test_that("vec_c() works correctly where c() fails", {
  expect_error(vec_c(1, new_ymon(1L)), class = "vctrs_error_incompatible_type")
})

test_that("vec_proxy() returns input", {
  expect_identical(vec_proxy(new_ymon(1:5)), new_ymon(1:5))
})
