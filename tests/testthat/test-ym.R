# ------------------------------------------------------------------------------
# helper

test_that("can create an empty ym()", {
  expect_equal(ym(), new_ym())
})

test_that("can create from just years", {
  expect_equal(ym(2019), ym(2019, 01))
})

test_that("can create from just months", {
  expect_equal(ym(month = 2), ym(0, 2))
})

test_that("can create from year and month", {
  expect_equal(ym(1970, 02), new_ym(31))
})

test_that("is vectorized", {
  expect_equal(ym(c(1970, 1970), c(02, 03)), new_ym(c(31, 59)))
})

test_that("year and month must be the same size (no recycling)", {
  expect_error(ym(1, c(1, 2)), "must be equal to the length")
})

test_that("year and month must be integer ish", {
  expect_error(ym(year = 1.5), class = "vctrs_error_cast_lossy")
  expect_error(ym(month = 1.5), class = "vctrs_error_cast_lossy")
})

test_that("year can be outside 0 and 9999", {
  expect_equal(ym(year = -1), new_ym(-719893))
  expect_equal(ym(year = 10000), new_ym(2932897))
})

test_that("month must be between 1 and 12", {
  expect_error(ym(month = 0), "`1` and `12`")
  expect_error(ym(month = 13), "`1` and `12`")
})

# ------------------------------------------------------------------------------
# new

test_that("can construct a ym prototype", {
  expect_length(new_ym(), 0L)
})

test_that("can construct new ym objects", {
  expect_s3_class(new_ym(0), "ym")
  expect_length(new_ym(c(0, 1)), 2)
})

test_that("can technically create invalid ym objects", {
  x <- new_ym(c(2, 1.5))
  expect_s3_class(x, "ym")
  expect_equal(vec_data(x), c(2, 1.5))
})

# ------------------------------------------------------------------------------
# is

test_that("can recognize ym() objects", {
  expect_true(is_ym(new_ym()))
  expect_false(is_ym(1))
  expect_false(is_ym(new_date()))
})

test_that("ym() objects are correctly not numeric", {
  expect_false(is.numeric(new_ym()))
})

test_that("ym() objects are Dates", {
  expect_s3_class(new_ym(), "Date")
})
