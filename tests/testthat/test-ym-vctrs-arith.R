# ------------------------------------------------------------------------------
# default

test_that("default method throws errors", {
  expect_error(vec_arith("+", new_ym(), "x"), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", "x", new_ym()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - ym

test_that("throws errors", {
  expect_error(vec_arith("+", new_ym(), new_ym()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - Date

test_that("throws errors", {
  expect_error(vec_arith("+", new_ym(), new_date()), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", new_date(), new_ym()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - POSIXct

test_that("throws errors", {
  expect_error(vec_arith("+", new_ym(), new_datetime()), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", new_datetime(), new_ym()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - POSIXlt

test_that("throws errors", {
  expect_error(vec_arith("+", new_ym(), as.POSIXlt(new_datetime())), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", as.POSIXlt(new_datetime()), new_ym()), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - MISSING

test_that("+ returns input", {
  expect_identical(+new_ym(), new_ym())
})

test_that("- throws errors", {
  expect_error(-new_ym(), class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------
# ym - integer
# ym - double

test_that("can add months", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_identical(new_ym(0L) + x, new_ym(1L))
    expect_identical(x + new_ym(0L), new_ym(1L))
  }
})

test_that("can subtract months", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_identical(new_ym(1L) - x, new_ym(0L))
  }
})

test_that("cannot subtract ym from value", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_error(x - new_ym(1L), class = "vctrs_error_incompatible_op")
  }
})

test_that("value must be 1D", {
  xs <- list(1L, 1)

  for (x in xs) {
    expect_error(matrix(x) + new_ym(1L), class = "vctrs_error_incompatible_op")
    expect_error(new_ym(1L) + matrix(x), class = "vctrs_error_incompatible_op")
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

    expect_identical(new_ym(c(0L, 0L)) + one, new_ym(c(1L, 1L)))
    expect_identical(one + new_ym(c(0L, 0L)), new_ym(c(1L, 1L)))

    expect_identical(new_ym(c(1L, 1L)) - one, new_ym(c(0L, 0L)))
    expect_error(one - new_ym(c(1L, 1L)), class = "vctrs_error_incompatible_op")

    expect_identical(new_ym(1L) + empty, new_ym())
    expect_identical(empty + new_ym(1L), new_ym())

    expect_identical(new_ym(1L) - empty, new_ym())
    expect_error(empty - new_ym(1L), class = "vctrs_error_incompatible_op")

    expect_error(new_ym(1:2) + vec, class = "vctrs_error_incompatible_size")
    expect_error(vec + new_ym(1:2), class = "vctrs_error_incompatible_size")

    expect_error(new_ym(1:2) - vec, class = "vctrs_error_incompatible_size")
    expect_error(vec - new_ym(1:2), class = "vctrs_error_incompatible_size")
  }
})

test_that("names are retained using base R rules", {
  ones <- list(1L, 1)
  twos <- list(c(1L, 2L), c(1, 2))

  for (i in seq_along(ones)) {
    one <- ones[[i]]
    two <- twos[[i]]

    # Same size, one is named
    expect_named(set_names(new_ym(1L), "x") + one, "x")
    expect_named(new_ym(1L) + set_names(one, "x"), "x")

    # Same size, both are named
    expect_named(set_names(new_ym(1L), "x") + set_names(one, "y"), "x")

    # Different size, longer is named
    expect_named(set_names(new_ym(1:2), c("x", "y")) + one, c("x", "y"))
    expect_named(new_ym(1L) + set_names(two, c("x", "y")), c("x", "y"))

    # Different size, shorter is named
    expect_named(new_ym(1:2) + set_names(one, "x"), NULL)
    expect_named(set_names(new_ym(1L), "x") + two, NULL)
  }
})

test_that("cannot add fractional doubles", {
  expect_error(new_ym(1L) + 1.5, class = "vctrs_error_cast_lossy")
  expect_error(1.5 + new_ym(1L), class = "vctrs_error_cast_lossy")
})
