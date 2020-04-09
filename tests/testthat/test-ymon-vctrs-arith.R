# ------------------------------------------------------------------------------
# default

test_that("default method throws errors", {
  expect_error(vec_arith("+", new_ymon(), "x"), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", "x", new_ymon()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - ymon

test_that("subtraction returns the number of months between", {
  expect_identical(new_ymon(1L) - new_ymon(0L), 1L)
})

test_that("tidy recycling rules", {
  expect_identical(new_ymon(c(1L, 2L)) - new_ymon(1L), c(0L, 1L))
  expect_identical(new_ymon(1L) - new_ymon(), integer())
  expect_error(new_ymon(1:2) - new_ymon(1:3), class = "vctrs_error_incompatible_size")
})

test_that("everything else throws errors", {
  expect_error(vec_arith("+", new_ymon(), new_ymon()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - Date

test_that("throws errors", {
  expect_error(vec_arith("+", new_ymon(), new_date()), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", new_date(), new_ymon()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - POSIXct

test_that("throws errors", {
  expect_error(vec_arith("+", new_ymon(), new_datetime()), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", new_datetime(), new_ymon()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - POSIXlt

test_that("throws errors", {
  expect_error(vec_arith("+", new_ymon(), as.POSIXlt(new_datetime())), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", as.POSIXlt(new_datetime()), new_ymon()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - MISSING

test_that("+ returns input", {
  expect_identical(+new_ymon(), new_ymon())
})

test_that("- throws errors", {
  expect_error(-new_ymon(), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ymon - integer
# ymon - double

test_that("can add months", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_identical(new_ymon(0L) + x, new_ymon(1L))
    expect_identical(x + new_ymon(0L), new_ymon(1L))
  }
})

test_that("can subtract months", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_identical(new_ymon(1L) - x, new_ymon(0L))
  }
})

test_that("cannot subtract ymon from value", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_error(x - new_ymon(1L), class = "vctrs_error_incompatible_op")
  }
})

test_that("value must be 1D", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_error(matrix(x) + new_ymon(1L), class = "vctrs_error_incompatible_op")
    expect_error(new_ymon(1L) + matrix(x), class = "vctrs_error_incompatible_op")
  }
})

test_that("tidy recycling rules", {
  ones <- list(1L, 1)
  empties <- list(integer(), numeric())
  vecs <- list(c(1L, 2L, 3L), c(1, 2, 3))

  for (i in seq_along(ones)) {
    one <- ones[[i]]
    empty <- empties[[i]]
    vec <- vecs[[i]]

    expect_identical(new_ymon(c(0L, 0L)) + one, new_ymon(c(1L, 1L)))
    expect_identical(one + new_ymon(c(0L, 0L)), new_ymon(c(1L, 1L)))

    expect_identical(new_ymon(c(1L, 1L)) - one, new_ymon(c(0L, 0L)))
    expect_error(one - new_ymon(c(1L, 1L)), class = "vctrs_error_incompatible_op")

    expect_identical(new_ymon(1L) + empty, new_ymon())
    expect_identical(empty + new_ymon(1L), new_ymon())

    expect_identical(new_ymon(1L) - empty, new_ymon())
    expect_error(empty - new_ymon(1L), class = "vctrs_error_incompatible_op")

    expect_error(new_ymon(1:2) + vec, class = "vctrs_error_incompatible_size")
    expect_error(vec + new_ymon(1:2), class = "vctrs_error_incompatible_size")

    expect_error(new_ymon(1:2) - vec, class = "vctrs_error_incompatible_size")
    expect_error(vec - new_ymon(1:2), class = "vctrs_error_incompatible_size")
  }
})

test_that("names are retained using base R rules", {
  ones <- list(1L, 1)
  twos <- list(c(1L, 2L), c(1, 2))

  for (i in seq_along(ones)) {
    one <- ones[[i]]
    two <- twos[[i]]

    # Same size, one is named
    expect_named(set_names(new_ymon(1L), "x") + one, "x")
    expect_named(new_ymon(1L) + set_names(one, "x"), "x")

    # Same size, both are named
    expect_named(set_names(new_ymon(1L), "x") + set_names(one, "y"), "x")

    # Different size, longer is named
    expect_named(set_names(new_ymon(1:2), c("x", "y")) + one, c("x", "y"))
    expect_named(new_ymon(1L) + set_names(two, c("x", "y")), c("x", "y"))

    # Different size, shorter is named
    expect_named(new_ymon(1:2) + set_names(one, "x"), NULL)
    expect_named(set_names(new_ymon(1L), "x") + two, NULL)
  }
})

test_that("cannot add fractional doubles", {
  expect_error(new_ymon(1L) + 1.5, class = "vctrs_error_cast_lossy")
  expect_error(1.5 + new_ymon(1L), class = "vctrs_error_cast_lossy")
})
