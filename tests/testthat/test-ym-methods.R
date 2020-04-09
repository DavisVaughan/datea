# ------------------------------------------------------------------------------
# lubridate

test_that("lubridate::tz() returns UTC", {
  expect_identical(lubridate::tz(new_ym(1L)), "UTC")
})

test_that("lubridate accessors work", {
  x <- new_ym(0L)

  expect_identical(lubridate::year(x), 1970)
  expect_identical(lubridate::month(x), 1)
  expect_identical(lubridate::day(x), 1L)
  expect_identical(lubridate::hour(x), 0L)
  expect_identical(lubridate::minute(x), 0L)
  expect_identical(lubridate::second(x), 0)
})

# ------------------------------------------------------------------------------
# vctrs backed methods

test_that("as.list() works", {
  x <- set_names(new_ym(1:2), c("x", "y"))

  expect <- list(
    set_names(new_ym(1L), "x"),
    set_names(new_ym(2L), "y")
  )

  expect_identical(as.list(x), expect)
})

test_that("c() works as best as it can", {
  expect_identical(c(new_ym(1L), new_ym(2L)), new_ym(1:2))

  # Nothing we can do about the second case
  expect_error(c(new_ym(1L), 1), class = "vctrs_error_incompatible_type")
  expect_identical(c(1, new_ym(1L)), c(1, 1))
})

test_that("vec_c() works correctly where c() fails", {
  expect_error(vec_c(1, new_ym(1L)), class = "vctrs_error_incompatible_type")
})

test_that("`[` works", {
  expect_identical(new_ym(1:2)[], new_ym(1:2))
  expect_identical(new_ym(1:2)[1], new_ym(1L))

  x <- set_names(new_ym(1:2), c("x", "y"))
  expect_identical(x["y"], set_names(new_ym(2L), "y"))
})

test_that("`[` has nice error message with extra indices", {
  expect_error(new_ym(1L)[1, 1], class = "rlib_error_dots_nonempty")
})

test_that("`[[` works", {
  x <- new_ym(1:2)
  expect_identical(x[[1]], new_ym(1L))

  # names are dropped
  x <- set_names(x, c("x", "y"))
  expect_identical(x[["y"]], new_ym(2L))
})
