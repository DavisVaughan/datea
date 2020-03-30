test_that("can format", {
  expect_identical(format(new_ym(1L)), "1970-02")
})

test_that("NA are formatted as character strings", {
  expect_identical(format(new_ym(NA_integer_)), "NA")
})

test_that("can format with different codes", {
  expect_identical(format(new_ym(1L), format = "%b %Y"), "Feb 1970")
})

test_that("known error when formatting outside the range of 0:9999", {
  expect_error(format(ym(-1, 1)))
  expect_error(format(ym(10000, 1)))
})

# ------------------------------------------------------------------------------

test_that("vctrs abbreviation is right", {
  expect_identical(vec_ptype_abbr(new_ym()), "ym")
})

test_that("vctrs full type is right", {
  expect_identical(vec_ptype_full(new_ym()), "ym")
})

# ------------------------------------------------------------------------------

test_that("ym print method has informative output", {
  verify_output(test_path("output", "test-ym-print.txt"), {
    "# zero length"
    new_ym()

    "# numbers are not quoted"
    new_ym(c(0L, 1L))

    "# NA values don't print like <NA>"
    new_ym(NA_integer_)

    "# years 0 < x < 1000 print with 4 digits, left pad with zeros"
    ym(1, 1)
    ym(10, 1)
    ym(100, 1)
  })
})
