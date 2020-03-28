test_that("vctrs abbreviation is right", {
  expect_identical(vec_ptype_abbr(new_ym()), "ym")
})

test_that("vctrs full type is right", {
  expect_identical(vec_ptype_full(new_ym()), "ym")
})

test_that("ym print method has informative output", {
  verify_output(test_path("output", "test-ym-print.txt"), {
    "# zero length"
    new_ym()

    "# numbers are not quoted"
    new_ym(c(0L, 1L))

    "# NA values don't print like <NA>"
    new_ym(NA_integer_)

    "# years >9999 print with 5 digits"
    ym(10000, 1)

    "# years 0 < x < 1000 print with 4 digits, left pad with zeros"
    ym(1, 1)
    ym(10, 1)
    ym(100, 1)

    "# years -1000 < x < 0 print with a negative and 4 digits"
    ym(-1, 1)
    ym(-10, 1)
    ym(-100, 1)
  })
})
