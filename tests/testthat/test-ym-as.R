
# ------------------------------------------------------------------------------
# ym -> ym

test_that("dots are checked", {
  expect_error(as_ym(new_ym(), 1))
})

test_that("can force", {
  expect_identical(as_ym(new_ym()), new_ym())
})

test_that("retains names", {
  expect_named(as_ym(set_names(new_ym(0L), "x")), "x")
})

# ------------------------------------------------------------------------------
# Date -> ym

test_that("can force", {
  expect_identical(as_ym(new_date(0)), new_ym(0L))
})

test_that("retains names", {
  expect_named(as_ym(set_names(new_date(0), "x")), "x")
})

test_that("force is allowed to be lossy", {
  expect_identical(as_ym(new_date(1)), new_ym(0L))
})

# ------------------------------------------------------------------------------
# POSIXct -> ym

test_that("can force", {
  expect_identical(as_ym(new_datetime(0, "UTC")), new_ym(0L))
})

test_that("retains names", {
  expect_named(as_ym(set_names(new_datetime(0), "x")), "x")
})

test_that("force is allowed to be lossy", {
  expect_identical(as_ym(new_datetime(1, "UTC")), new_ym(0L))
})

test_that("time zone doesn't matter", {
  expect_identical(
    as_ym(new_datetime(0, "UTC")),
    as_ym(new_datetime(0, "America/New_York"))
  )
})

# ------------------------------------------------------------------------------
# POSIXlt -> ym

test_that("can force", {
  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_identical(as_ym(datetime), new_ym(0L))
})

test_that("retains names", {
  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_named(as_ym(set_names(datetime, "x")), "x")
})

test_that("force is allowed to be lossy", {
  datetime <- as.POSIXlt("1970-01-01 01:01:01", tz = "UTC")
  expect_identical(as_ym(datetime), new_ym(0L))
})

test_that("time zone doesn't matter", {
  expect_identical(
    as_ym(as.POSIXlt("1970-01-01", tz = "UTC")),
    as_ym(as.POSIXlt("1970-01-01", tz = "America/New_York"))
  )
})

# ------------------------------------------------------------------------------
# ym -> Date

test_that("dots are checked", {
  expect_error(as.Date(new_ym(), 1))
})

test_that("can force", {
  expect_identical(as.Date(new_ym(0L)), new_date(0))
  expect_identical(as.Date(new_ym(1L)), new_date(31))
})

test_that("retains names", {
  expect_named(as.Date(set_names(new_ym(0L), "x")), "x")
})

# ------------------------------------------------------------------------------
# ym -> POSIXct

test_that("dots are checked", {
  expect_error(as.POSIXct(new_ym(), tz = "UTC", 1))
})

test_that("can force", {
  expect_identical(as.POSIXct(new_ym(0L)), as.POSIXct("1970-01-01", tz = "UTC"))
  expect_identical(as.POSIXct(new_ym(1L)), as.POSIXct("1970-02-01", tz = "UTC"))
})

test_that("retains names", {
  expect_named(as.POSIXct(set_names(new_ym(0L), "x")), "x")
})

test_that("default tz is UTC", {
  expect_identical(attr(as.POSIXct(new_ym(0L)), "tzone", exact = TRUE), "UTC")
})

test_that("can modify time zone and keep clock time", {
  expect_identical(as.POSIXct(new_ym(0L), tz = "America/New_York"), as.POSIXct("1970-01-01", tz = "America/New_York"))
  expect_identical(as.POSIXct(new_ym(1L), tz = "America/New_York"), as.POSIXct("1970-02-01", tz = "America/New_York"))
})

# ------------------------------------------------------------------------------
# ym -> POSIXlt

test_that("dots are checked", {
  expect_error(as.POSIXlt(new_ym(), tz = "UTC", 1))
})

test_that("can force", {
  expect_identical(as.POSIXlt(new_ym(0L)), as.POSIXlt("1970-01-01", tz = "UTC"))
  expect_identical(as.POSIXlt(new_ym(1L)), as.POSIXlt("1970-02-01", tz = "UTC"))
})

test_that("retains names", {
  expect_named(as.POSIXlt(set_names(new_ym(0L), "x")), "x")
})

test_that("default tz is UTC", {
  expect_identical(attr(as.POSIXlt(new_ym(0L)), "tzone", exact = TRUE), "UTC")
})

test_that("can modify time zone and keep clock time", {
  expect_identical(as.POSIXlt(new_ym(0L), tz = "America/New_York"), as.POSIXlt("1970-01-01", tz = "America/New_York"))
  expect_identical(as.POSIXlt(new_ym(1L), tz = "America/New_York"), as.POSIXlt("1970-02-01", tz = "America/New_York"))
})

# ------------------------------------------------------------------------------

test_that("`as_ym()` gives informative errors", {
  verify_output(test_path("errors", "test-ym-as.txt"), {
    "# default method collapses classes"
    as_ym(ordered(factor("x")))
  })
})
