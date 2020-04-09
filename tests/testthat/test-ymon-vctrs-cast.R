# ------------------------------------------------------------------------------
# default

test_that("default method throws errors", {
  expect_error(vec_cast(new_ymon(), 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1, new_ymon()), class = "vctrs_error_incompatible_type")
})

# ------------------------------------------------------------------------------
# ymon - ymon

test_that("safe casts", {
  expect_identical(vec_cast(new_ymon(), new_ymon()), new_ymon())

  expect_identical(vec_cast(new_ymon(NA_integer_), new_ymon()), new_ymon(NA_integer_))
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ymon(0L), "x"), new_ymon()), "x")
})

# ------------------------------------------------------------------------------
# ymon - Date

test_that("safe casts", {
  ymon <- new_ymon()
  date <- new_date()

  expect_identical(vec_cast(ymon, date), date)
  expect_identical(vec_cast(date, ymon), ymon)

  missing_date <- new_date(NA_real_)
  missing_ymon <- new_ymon(NA_integer_)

  expect_identical(vec_cast(missing_date, missing_ymon), missing_ymon)
  expect_identical(vec_cast(missing_ymon, missing_date), missing_date)
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ymon(0L), "x"), new_date()), "x")
  expect_named(vec_cast(set_names(new_date(0), "x"), new_ymon()), "x")
})

test_that("cast to ymon must be lossless", {
  expect_error(vec_cast(as.Date("1970-01-02"), new_ymon()), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
# ymon - POSIXct

test_that("safe casts", {
  ymon <- new_ymon()
  datetime <- new_datetime(tzone = "UTC")

  expect_identical(vec_cast(ymon, datetime), datetime)
  expect_identical(vec_cast(datetime, ymon), ymon)

  missing_datetime <- new_datetime(NA_real_, tzone = "UTC")
  missing_ymon <- new_ymon(NA_integer_)

  expect_identical(vec_cast(missing_datetime, missing_ymon), missing_ymon)
  expect_identical(vec_cast(missing_ymon, missing_datetime), missing_datetime)
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ymon(0L), "x"), new_datetime(tzone = "UTC")), "x")
  expect_named(vec_cast(set_names(new_datetime(0, tzone = "UTC"), "x"), new_ymon()), "x")
})

test_that("cast to ymon must be lossless", {
  datetime <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ymon()), class = "vctrs_error_cast_lossy")

  datetime <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ymon()), class = "vctrs_error_cast_lossy")
})

test_that("exact underlying time can be round tripped", {
  datetime <- as.POSIXct("1970-02-01", tz = "America/New_York")

  ymon <- vec_cast(datetime, new_ymon())
  expect_identical(ymon, new_ymon(1L))

  datetime2 <- vec_cast(ymon, datetime)
  expect_identical(datetime2, datetime)
})

# ------------------------------------------------------------------------------
# ymon - POSIXlt

test_that("safe casts", {
  ymon <- new_ymon()
  datetime <- as.POSIXlt(new_datetime(tzone = "UTC"))

  expect_identical(vec_cast(ymon, datetime), datetime)
  expect_identical(vec_cast(datetime, ymon), ymon)

  missing_datetime <- as.POSIXlt(new_datetime(NA_real_, tzone = "UTC"))
  missing_ymon <- new_ymon(NA_integer_)

  expect_identical(vec_cast(missing_datetime, missing_ymon), missing_ymon)
  expect_identical(vec_cast(missing_ymon, missing_datetime), missing_datetime)
})

test_that("retains names", {
  datetime <- as.POSIXlt(new_datetime(tzone = "UTC"))
  expect_named(vec_cast(set_names(new_ymon(0L), "x"), datetime), "x")

  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_named(vec_cast(set_names(datetime, "x"), new_ymon()), "x")
})

test_that("cast to ymon must be lossless", {
  datetime <- as.POSIXlt("1970-01-01 01:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ymon()), class = "vctrs_error_cast_lossy")

  datetime <- as.POSIXlt("1970-01-02 00:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ymon()), class = "vctrs_error_cast_lossy")
})

test_that("exact underlying time can be round tripped", {
  datetime <- as.POSIXlt("1970-02-01", tz = "America/New_York")

  ymon <- vec_cast(datetime, new_ymon())
  expect_identical(ymon, new_ymon(1L))

  datetime2 <- vec_cast(ymon, datetime)
  expect_identical(datetime2, datetime)
})
