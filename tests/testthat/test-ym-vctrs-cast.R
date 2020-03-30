# ------------------------------------------------------------------------------
# default

test_that("default method throws errors", {
  expect_error(vec_cast(new_ym(), 1), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(1, new_ym()), class = "vctrs_error_incompatible_cast")
})

# ------------------------------------------------------------------------------
# ym - ym

test_that("safe casts", {
  expect_identical(vec_cast(new_ym(), new_ym()), new_ym())

  expect_identical(vec_cast(new_ym(NA_integer_), new_ym()), new_ym(NA_integer_))
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ym(0L), "x"), new_ym()), "x")
})

# ------------------------------------------------------------------------------
# ym - Date

test_that("safe casts", {
  ym <- new_ym()
  date <- new_date()

  expect_identical(vec_cast(ym, date), date)
  expect_identical(vec_cast(date, ym), ym)

  missing_date <- new_date(NA_real_)
  missing_ym <- new_ym(NA_integer_)

  expect_identical(vec_cast(missing_date, missing_ym), missing_ym)
  expect_identical(vec_cast(missing_ym, missing_date), missing_date)
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ym(0L), "x"), new_date()), "x")
  expect_named(vec_cast(set_names(new_date(0), "x"), new_ym()), "x")
})

test_that("cast to ym must be lossless", {
  expect_error(vec_cast(as.Date("1970-01-02"), new_ym()), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
# ym - POSIXct

test_that("safe casts", {
  ym <- new_ym()
  datetime <- new_datetime(tzone = "UTC")

  expect_identical(vec_cast(ym, datetime), datetime)
  expect_identical(vec_cast(datetime, ym), ym)

  missing_datetime <- new_datetime(NA_real_, tzone = "UTC")
  missing_ym <- new_ym(NA_integer_)

  expect_identical(vec_cast(missing_datetime, missing_ym), missing_ym)
  expect_identical(vec_cast(missing_ym, missing_datetime), missing_datetime)
})

test_that("retains names", {
  expect_named(vec_cast(set_names(new_ym(0L), "x"), new_datetime(tzone = "UTC")), "x")
  expect_named(vec_cast(set_names(new_datetime(0, tzone = "UTC"), "x"), new_ym()), "x")
})

test_that("cast to ym must be lossless", {
  datetime <- as.POSIXct("1970-01-01 01:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ym()), class = "vctrs_error_cast_lossy")

  datetime <- as.POSIXct("1970-01-02 00:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ym()), class = "vctrs_error_cast_lossy")
})

test_that("exact underlying time can be round tripped", {
  datetime <- as.POSIXct("1970-02-01", tz = "America/New_York")

  ym <- vec_cast(datetime, new_ym())
  expect_identical(ym, new_ym(1L))

  datetime2 <- vec_cast(ym, datetime)
  expect_identical(datetime2, datetime)
})

# ------------------------------------------------------------------------------
# ym - POSIXlt

test_that("safe casts", {
  ym <- new_ym()
  datetime <- as.POSIXlt(character(), tz = "UTC")

  expect_identical(vec_cast(ym, datetime), datetime)
  expect_identical(vec_cast(datetime, ym), ym)

  missing_datetime <- as.POSIXlt(NA_character_, tz = "UTC")
  missing_ym <- new_ym(NA_integer_)

  expect_identical(vec_cast(missing_datetime, missing_ym), missing_ym)
  expect_identical(vec_cast(missing_ym, missing_datetime), missing_datetime)
})

test_that("retains names", {
  datetime <- as.POSIXlt(character(), tz = "UTC")
  expect_named(vec_cast(set_names(new_ym(0L), "x"), datetime), "x")

  datetime <- as.POSIXlt("1970-01-01", tz = "UTC")
  expect_named(vec_cast(set_names(datetime, "x"), new_ym()), "x")
})

test_that("cast to ym must be lossless", {
  datetime <- as.POSIXlt("1970-01-01 01:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ym()), class = "vctrs_error_cast_lossy")

  datetime <- as.POSIXlt("1970-01-02 00:00:00", tz = "UTC")
  expect_error(vec_cast(datetime, new_ym()), class = "vctrs_error_cast_lossy")
})

test_that("exact underlying time can be round tripped", {
  datetime <- as.POSIXlt("1970-02-01", tz = "America/New_York")

  ym <- vec_cast(datetime, new_ym())
  expect_identical(ym, new_ym(1L))

  datetime2 <- vec_cast(ym, datetime)
  expect_identical(datetime2, datetime)
})
