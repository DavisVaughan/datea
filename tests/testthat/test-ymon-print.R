test_that("can format", {
  expect_identical(format(new_ymon(1L)), "1970-02")
})

test_that("retains names", {
  expect_named(format(set_names(new_ymon(1L), "x")), "x")
})

test_that("NA are formatted as character strings", {
  expect_identical(format(new_ymon(NA_integer_)), "NA")
})

test_that("can format outside the range of 0:9999", {
  expect_identical(format(ymon(-1, 1)), "-0001-01")
  expect_identical(format(ymon(10000, 1)), "10000-01")
})

# ------------------------------------------------------------------------------

test_that("vctrs abbreviation is right", {
  expect_identical(vec_ptype_abbr(new_ymon()), "ymon")
})

test_that("vctrs full type is right", {
  expect_identical(vec_ptype_full(new_ymon()), "ymon")
})

# ------------------------------------------------------------------------------

test_that("ymon print method has informative output", {
  verify_output(test_path("output", "test-ymon-print.txt"), {
    "# zero length"
    new_ymon()

    "# numbers are not quoted"
    new_ymon(c(0L, 1L))

    "# NA values don't print like <NA>"
    new_ymon(NA_integer_)

    "# years 0 < x < 1000 print with 4 digits, left pad with zeros"
    ymon(1, 1)
    ymon(10, 1)
    ymon(100, 1)

    "# names are printed"
    set_names(new_ymon(0L), "x")
  })
})
