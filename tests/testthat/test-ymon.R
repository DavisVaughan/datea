# ------------------------------------------------------------------------------
# helper

test_that("can create an empty ymon()", {
  expect_identical(ymon(year = integer(), month = integer()), new_ymon())
})

test_that("can create from year and month", {
  expect_identical(ymon(1970, 2), new_ymon(1L))
  expect_identical(ymon(1969, 12), new_ymon(-1L))
})

test_that("recycles to common size", {
  expect_identical(ymon(c(1970, 1971), 01), new_ymon(c(0L, 12L)))
})

test_that("year and month must be integer ish", {
  expect_error(ymon(year = 1.5, month = 1), class = "vctrs_error_cast_lossy")
  expect_error(ymon(year = 1, month = 1.5), class = "vctrs_error_cast_lossy")
})

test_that("year can be outside 0 and 9999", {
  expect_identical(ymon(year = -1, month = 1), new_ymon(-23652L))
  expect_identical(ymon(year = 10000, month = 1), new_ymon(96360L))
})

test_that("month must be between 1 and 12", {
  expect_error(ymon(year = 1, month = 0), "`1` and `12`")
  expect_error(ymon(year = 1, month = 13), "`1` and `12`")
})

test_that("`NA` month or year propagate", {
  expect_identical(ymon(NA, 1), new_ymon(NA_integer_))
  expect_identical(ymon(1, NA), new_ymon(NA_integer_))
})

# ------------------------------------------------------------------------------
# new

test_that("can construct a ymon prototype", {
  expect_length(new_ymon(), 0L)
})

test_that("can construct new ymon objects", {
  expect_s3_class(new_ymon(0L), "ymon")
  expect_length(new_ymon(c(0L, 1L)), 2)
})

test_that("ymon must be an integer", {
  expect_error(new_ymon(1), class = "vctrs_error_assert_ptype")
})

test_that("ymon can be `NA`", {
  expect_length(new_ymon(NA_integer_), 1L)
})

test_that("names are retained", {
  expect_named(new_ymon(c(x = 1L)), "x")
})

# ------------------------------------------------------------------------------
# is

test_that("can recognize ymon() objects", {
  expect_true(is_ymon(new_ymon()))
  expect_false(is_ymon(1))
  expect_false(is_ymon(new_date()))
})

test_that("ymon() objects are technically seen as numeric", {
  expect_true(is.numeric(new_ymon()))
})

# ------------------------------------------------------------------------------
# accessors

test_that("accessors errors on non-ymon objects", {
  expect_error(ymon_year(1))
  expect_error(ymon_month(1))
})

test_that("can access components", {
  x <- ymon(c(2000, 2001), c(1, 2))
  expect_identical(ymon_year(x), c(2000L, 2001L))
  expect_identical(ymon_month(x), c(1L, 2L))
})

test_that("NA propagates", {
  x <- new_ymon(NA_integer_)
  expect_identical(ymon_year(x), NA_integer_)
  expect_identical(ymon_month(x), NA_integer_)
})

test_that("names are not carried along", {
  x <- set_names(new_ymon(0L), "x")
  expect_named(ymon_year(x), NULL)
  expect_named(ymon_month(x), NULL)
})

# ------------------------------------------------------------------------------

test_that("ymon helpers have informative output", {
  verify_output(test_path("errors", "test-ymon.txt"), {
    "# accessors errors on non-ymon objects"
    ymon_year(1)
    ymon_month(1)
  })
})

