
# ------------------------------------------------------------------------------
# ymon -> ymon

test_that("dots are checked", {
  expect_error(as_ymon(new_ymon(), 1))
})

test_that("can force", {
  expect_identical(as_ymon(new_ymon()), new_ymon())
})

test_that("retains names", {
  expect_named(as_ymon(set_names(new_ymon(0L), "x")), "x")
})

# ------------------------------------------------------------------------------
# Date -> ymon

test_that("can force", {
  expect_identical(as_ymon(new_date(0)), new_ymon(0L))
})

test_that("retains names", {
  expect_named(as_ymon(set_names(new_date(0), "x")), "x")
})

test_that("force is allowed to be lossy", {
  expect_identical(as_ymon(new_date(1)), new_ymon(0L))
})

# ------------------------------------------------------------------------------
# POSIXct -> ymon

test_that("can force", {
  expect_identical(as_ymon(new_datetime(0, "UTC")), new_ymon(0L))
})

test_that("retains names", {
  expect_named(as_ymon(set_names(new_datetime(0), "x")), "x")
})

test_that("force is allowed to be lossy", {
  expect_identical(as_ymon(new_datetime(1, "UTC")), new_ymon(0L))
})

test_that("time zone doesn't matter", {
  expect_identical(
    as_ymon(new_datetime(0, "UTC")),
    as_ymon(new_datetime(0, "America/New_York"))
  )
})

# ------------------------------------------------------------------------------
# POSIXlt -> ymon

test_that("can force", {
  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_identical(as_ymon(datetime), new_ymon(0L))
})

test_that("retains names", {
  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_named(as_ymon(set_names(datetime, "x")), "x")
})

test_that("force is allowed to be lossy", {
  datetime <- as.POSIXlt("1970-01-01 01:01:01", tz = "UTC")
  expect_identical(as_ymon(datetime), new_ymon(0L))
})

test_that("time zone doesn't matter", {
  expect_identical(
    as_ymon(as.POSIXlt("1970-01-01", tz = "UTC")),
    as_ymon(as.POSIXlt("1970-01-01", tz = "America/New_York"))
  )
})

# ------------------------------------------------------------------------------
# integer -> ymon

test_that("can force", {
  expect_identical(as_ymon(1L), new_ymon(1L))
})

test_that("retains names", {
  expect_named(as_ymon(set_names(1L, "x")), "x")
})

# ------------------------------------------------------------------------------
# double -> ymon

test_that("can force", {
  expect_identical(as_ymon(1), new_ymon(1L))
})

test_that("retains names", {
  expect_named(as_ymon(set_names(1, "x")), "x")
})

test_that("cannot have lossy double to integer conversion", {
  expect_error(as_ymon(1.5), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
# character -> ymon

test_that("can force", {
  expect_identical(as_ymon("1970-01"), new_ymon(0L))
})

test_that("size zero character can be converted", {
  expect_identical(as_ymon(character()), new_ymon())
})

test_that("NA input can be converted", {
  expect_identical(as_ymon(NA_character_), new_ymon(NA_integer_))
  expect_identical(as_ymon(c(NA, "1970-01")), new_ymon(c(NA, 0L)))
})

test_that("retains names", {
  expect_named(as_ymon(set_names("1970-01", "x")), "x")
})

test_that("can convert with < 4 year digits", {
  expect_identical(as_ymon("1-01"), ymon(1, 1))
  expect_identical(as_ymon("0001-01"), ymon(1, 1))
})

test_that("can convert with > 4 year digits", {
  expect_identical(as_ymon("10000-01"), ymon(10000, 1))
})

test_that("can convert with < 2 month digits", {
  expect_identical(as_ymon("1970-1"), new_ymon(0L))
})

test_that("must have a dash", {
  verify_errors({
    expect_error(as_ymon("11"))
  })
})

test_that("can only have 1 dash", {
  verify_errors({
    expect_error(as_ymon("2019-01-01"))
  })
})

test_that("catches non-parsable year/month components", {
  verify_errors({
    expect_error(as_ymon("foo-01"))
    expect_error(as_ymon("1970-foo"))
    expect_error(as_ymon("NA-01"))
    expect_error(as_ymon("1970-NA"))
  })
})

test_that("fractional double can't be converted", {
  verify_errors({
    expect_error(as_ymon("2019.5-01"), class = "vctrs_error_cast_lossy")
  })
})

test_that("OOB months are caught", {
  verify_errors({
    expect_error(as_ymon("2019-13"))
  })
})

test_that("multiple failures are truncated nicely", {
  verify_errors({
    expect_error(as_ymon(rep("x", 5)))
    expect_error(as_ymon(rep("x", 20)))
  })
})

# ------------------------------------------------------------------------------
# ymon -> Date

test_that("dots are checked", {
  expect_error(as.Date(new_ymon(), 1))
})

test_that("can force", {
  expect_identical(as.Date(new_ymon(0L)), new_date(0))
  expect_identical(as.Date(new_ymon(1L)), new_date(31))
})

test_that("retains names", {
  expect_named(as.Date(set_names(new_ymon(0L), "x")), "x")
})

# ------------------------------------------------------------------------------
# ymon -> POSIXct

test_that("dots are checked", {
  expect_error(as.POSIXct(new_ymon(), tz = "UTC", 1))
})

test_that("can force", {
  expect_identical(as.POSIXct(new_ymon(0L)), as.POSIXct("1970-01-01", tz = "UTC"))
  expect_identical(as.POSIXct(new_ymon(1L)), as.POSIXct("1970-02-01", tz = "UTC"))
})

test_that("retains names", {
  expect_named(as.POSIXct(set_names(new_ymon(0L), "x")), "x")
})

test_that("default tz is UTC", {
  expect_identical(attr(as.POSIXct(new_ymon(0L)), "tzone", exact = TRUE), "UTC")
})

test_that("can modify time zone and keep clock time", {
  expect_identical(as.POSIXct(new_ymon(0L), tz = "America/New_York"), as.POSIXct("1970-01-01", tz = "America/New_York"))
  expect_identical(as.POSIXct(new_ymon(1L), tz = "America/New_York"), as.POSIXct("1970-02-01", tz = "America/New_York"))
})

# ------------------------------------------------------------------------------
# ymon -> POSIXlt

test_that("dots are checked", {
  expect_error(as.POSIXlt(new_ymon(), tz = "UTC", 1))
})

test_that("can force", {
  expect_identical(as.POSIXlt(new_ymon(0L)), as.POSIXlt("1970-01-01", tz = "UTC"))
  expect_identical(as.POSIXlt(new_ymon(1L)), as.POSIXlt("1970-02-01", tz = "UTC"))
})

test_that("retains names", {
  expect_named(as.POSIXlt(set_names(new_ymon(0L), "x")), "x")
})

test_that("default tz is UTC", {
  expect_identical(attr(as.POSIXlt(new_ymon(0L)), "tzone", exact = TRUE), "UTC")
})

test_that("can modify time zone and keep clock time", {
  expect_identical(as.POSIXlt(new_ymon(0L), tz = "America/New_York"), as.POSIXlt("1970-01-01", tz = "America/New_York"))
  expect_identical(as.POSIXlt(new_ymon(1L), tz = "America/New_York"), as.POSIXlt("1970-02-01", tz = "America/New_York"))
})

# ------------------------------------------------------------------------------
# ymon -> character

test_that("dots are checked", {
  expect_error(as.character(new_ymon(), 1))
})

test_that("can force", {
  expect_identical(as.character(new_ymon(0L)), "1970-01")
  expect_identical(as.character(new_ymon(11L)), "1970-12")
})

test_that("retains names", {
  expect_named(as.character(set_names(new_ymon(0L), "x")), "x")
})

test_that("NA are converted to NA_character_", {
  expect_identical(as.character(new_ymon(NA_integer_)), NA_character_)
})

test_that("year >=10000 are fully printed", {
  expect_identical(as.character(ymon(10000, 1)), "10000-01")
})

test_that("negative years retain negative sign and print with at least 4 digits", {
  expect_identical(as.character(ymon(-1, 1)), "-0001-01")
  expect_identical(as.character(ymon(-10000, 1)), "-10000-01")
})

# ------------------------------------------------------------------------------
# ymon -> integer

test_that("dots are checked", {
  expect_error(as.integer(new_ymon(), 1))
})

test_that("can force", {
  expect_identical(as.integer(new_ymon(0L)), 0L)
  expect_identical(as.integer(new_ymon(11L)), 11L)
})

test_that("retains names", {
  expect_named(as.integer(set_names(new_ymon(0L), "x")), "x")
})

# ------------------------------------------------------------------------------
# ymon -> double

test_that("dots are checked", {
  expect_error(as.double(new_ymon(), 1))
})

test_that("can force", {
  expect_identical(as.double(new_ymon(0L)), 0)
  expect_identical(as.double(new_ymon(11L)), 11)
})

test_that("retains names", {
  expect_named(as.double(set_names(new_ymon(0L), "x")), "x")
})

# ------------------------------------------------------------------------------

test_that("`as_ymon()` gives informative errors", {
  verify_output(test_path("errors", "test-ymon-as.txt"), {
    "# default method collapses classes"
    as_ymon(ordered(factor("x")))

    "# must have a dash"
    as_ymon("11")

    "# can only have 1 dash"
    as_ymon("2019-01-01")

    "# catches non-parsable year/month components"
    as_ymon("foo-01")
    as_ymon("1970-foo")
    as_ymon("NA-01")
    as_ymon("1970-NA")

    "# fractional double can't be converted"
    as_ymon("2019.5-01")

    "# OOB months are caught"
    as_ymon("2019-13")

    "# multiple failures are truncated nicely"
    as_ymon(rep("x", 5))
    as_ymon(rep("x", 20))
  })
})
