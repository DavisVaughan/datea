test_that("can alter the format", {
  expect_identical(ymon_parse("1970 Jan", format = "%Y %b"), new_ymon(0L))
})

test_that("`NA` parses fine", {
  expect_identical(ymon_parse(NA_character_), new_ymon(NA_integer_))
})

test_that("empty character parses", {
  expect_identical(ymon_parse(character()), new_ymon(integer()))
})

test_that("throws warnings on failure to parse", {
  expect_identical(expect_warning(ymon_parse("1970")), new_ymon(NA_integer_))
  expect_identical(expect_warning(ymon_parse(c(rep("1970", 2), "1970-01", "1970"))), new_ymon(c(NA_integer_, NA_integer_, 0L, NA_integer_)))
  expect_identical(expect_warning(ymon_parse(rep("1970", 6))), new_ymon(rep(NA_integer_, 6)))
})

test_that("`ymon_parse()` gives informative errors", {
  verify_output(test_path("errors", "test-ymon-parse.txt"), {
    "# throws warnings on failure to parse"
    ymon_parse("1970")
    ymon_parse(c(rep("1970", 2), "1970-01", "1970"))
    ymon_parse(rep("1970", 6))
  })
})
