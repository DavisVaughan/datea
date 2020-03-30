test_that("can alter the format", {
  expect_identical(ym_parse("1970 Jan", format = "%Y %b"), new_ym(0L))
})

test_that("`NA` parses fine", {
  expect_identical(ym_parse(NA_character_), new_ym(NA_integer_))
})

test_that("empty character parses", {
  expect_identical(ym_parse(character()), new_ym(integer()))
})

test_that("throws warnings on failure to parse", {
  expect_identical(expect_warning(ym_parse("1970")), new_ym(NA_integer_))
  expect_identical(expect_warning(ym_parse(c(rep("1970", 2), "1970-01", "1970"))), new_ym(c(NA_integer_, NA_integer_, 0L, NA_integer_)))
  expect_identical(expect_warning(ym_parse(rep("1970", 6))), new_ym(rep(NA_integer_, 6)))
})

test_that("`ym_parse()` gives informative errors", {
  verify_output(test_path("errors", "test-ym-parse.txt"), {
    "# throws warnings on failure to parse"
    ym_parse("1970")
    ym_parse(c(rep("1970", 2), "1970-01", "1970"))
    ym_parse(rep("1970", 6))
  })
})
