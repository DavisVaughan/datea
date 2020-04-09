test_that("seq() validates from", {
  expect_error(seq(new_ymon(1:2)), class = "vctrs_error_assert_size")
  expect_error(seq(new_ymon(NA_integer_)), "can't be `NA`")
})

test_that("seq() validates length.out / along.with exclusiveness", {
  expect_error(seq(new_ymon(1L), length.out = 1, along.with = 2), "one of")
})

test_that("seq() only takes two optional args", {
  x <- new_ymon(1L)
  expect_error(seq(x, to = new_ymon(1), by = 1, length.out = 1), "exactly two")
  expect_error(seq(x, to = new_ymon(1), by = 1, along.with = 1), "exactly two")
})

test_that("seq() requires two optional args", {
  x <- new_ymon(1L)
  expect_error(seq(x, to = new_ymon(1)), "exactly two")
  expect_error(seq(x, by = 1), "exactly two")
  expect_error(seq(x, length.out = 1), "exactly two")
  expect_error(seq(x, along.with = 1), "exactly two")
})

test_that("seq() validates `to`", {
  expect_error(seq(new_ymon(1L), to = new_ymon(1:2), by = 1), class = "vctrs_error_assert_size")
  expect_error(seq(new_ymon(1L), to = 1, by = 1), "must be a ymon")
  expect_error(seq(new_ymon(1L), to = new_ymon(NA_integer_), by = 1), "can't be `NA`")
})

test_that("seq() validates `by`", {
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), by = 1:2), class = "vctrs_error_assert_size")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), by = NA_integer_), "can't be `NA`")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), by = 0), "can't be `0`")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), by = "x"), class = "vctrs_error_incompatible_type")
})

test_that("seq() validates `length.out`", {
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), length.out = 1:2), class = "vctrs_error_assert_size")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), length.out = NA_integer_), "can't be `NA`")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), length.out = -1), "can't be negative")
  expect_error(seq(new_ymon(1L), to = new_ymon(1L), length.out = "x"), class = "vctrs_error_incompatible_type")
})

test_that("seq() validates from/to/by signs", {
  expect_error(seq(new_ymon(1L), to = new_ymon(2L), by = -1), "must be positive")
  expect_error(seq(new_ymon(2L), to = new_ymon(1L), by = 1), "must be negative")
})

test_that("seq() enforces non-fractional results", {
  expect_error(seq(new_ymon(1L), to = new_ymon(2L), length.out = 3), "non-fractional")
})

test_that("seq(to, by) works", {
  expect_identical(seq(new_ymon(0L), to = new_ymon(4L), by = 2), new_ymon(c(0L, 2L, 4L)))
  expect_identical(seq(new_ymon(0L), to = new_ymon(5L), by = 2), new_ymon(c(0L, 2L, 4L)))

  expect_identical(seq(new_ymon(0L), to = new_ymon(-5L), by = -2), new_ymon(c(0L, -2L, -4L)))
})

test_that("seq(to, length.out) works", {
  expect_identical(seq(new_ymon(0L), to = new_ymon(4L), length.out = 2), new_ymon(c(0L, 4L)))
  expect_identical(seq(new_ymon(0L), to = new_ymon(4L), length.out = 1), new_ymon(c(0L)))
  expect_identical(seq(new_ymon(0L), to = new_ymon(4L), length.out = 5), new_ymon(c(0:4)))

  expect_identical(seq(new_ymon(0L), to = new_ymon(4L), along.with = 1:2), new_ymon(c(0L, 4L)))
})

test_that("seq(by, length.out) works", {
  expect_identical(seq(new_ymon(0L), by = 2, length.out = 3), new_ymon(c(0L, 2L, 4L)))
  expect_identical(seq(new_ymon(0L), by = -2, length.out = 3), new_ymon(c(0L, -2L, -4L)))

  expect_identical(seq(new_ymon(0L), by = 2, along.with = 1:3), new_ymon(c(0L, 2L, 4L)))
})
