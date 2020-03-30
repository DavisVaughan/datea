# ------------------------------------------------------------------------------
# helper

test_that("can create an empty ym()", {
  expect_identical(ym(year = integer(), month = integer()), new_ym())
})

test_that("can create from year and month", {
  expect_identical(ym(1970, 2), new_ym(1L))
  expect_identical(ym(1969, 12), new_ym(-1L))
})

test_that("recycles to common size", {
  expect_identical(ym(c(1970, 1971), 01), new_ym(c(0L, 12L)))
})

test_that("year and month must be integer ish", {
  expect_error(ym(year = 1.5, month = 1), class = "vctrs_error_cast_lossy")
  expect_error(ym(year = 1, month = 1.5), class = "vctrs_error_cast_lossy")
})

test_that("year can be outside 0 and 9999", {
  expect_identical(ym(year = -1, month = 1), new_ym(-23652L))
  expect_identical(ym(year = 10000, month = 1), new_ym(96360L))
})

test_that("month must be between 1 and 12", {
  expect_error(ym(year = 1, month = 0), "`1` and `12`")
  expect_error(ym(year = 1, month = 13), "`1` and `12`")
})

test_that("`NA` month or year propagate", {
  expect_identical(ym(NA, 1), new_ym(NA_integer_))
  expect_identical(ym(1, NA), new_ym(NA_integer_))
})

# ------------------------------------------------------------------------------
# new

test_that("can construct a ym prototype", {
  expect_length(new_ym(), 0L)
})

test_that("can construct new ym objects", {
  expect_s3_class(new_ym(0L), "ym")
  expect_length(new_ym(c(0L, 1L)), 2)
})

test_that("ym must be an integer", {
  expect_error(new_ym(1), class = "vctrs_error_assert_ptype")
})

test_that("ym can be `NA`", {
  expect_length(new_ym(NA_integer_), 1L)
})

test_that("names are retained", {
  expect_named(new_ym(c(x = 1L)), "x")
})

# ------------------------------------------------------------------------------
# is

test_that("can recognize ym() objects", {
  expect_true(is_ym(new_ym()))
  expect_false(is_ym(1))
  expect_false(is_ym(new_date()))
})

test_that("ym() objects are technically seen as numeric", {
  expect_true(is.numeric(new_ym()))
})
