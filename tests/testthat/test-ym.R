# ------------------------------------------------------------------------------
# helper



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
