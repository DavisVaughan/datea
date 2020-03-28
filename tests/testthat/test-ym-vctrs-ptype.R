test_that("default method", {
  expect_error(vec_ptype2(new_ym(), 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(1, new_ym()), class = "vctrs_error_incompatible_type")
})

test_that("unspecified method", {
  expect_identical(vec_ptype2(new_ym(), NA), new_ym())
  expect_identical(vec_ptype2(NA, new_ym()), new_ym())
})

test_that("ym - ym", {
  expect_identical(vec_ptype2(new_ym(), new_ym()), new_ym())
})

test_that("ym - Date", {
  expect_identical(vec_ptype2(new_ym(), new_date()), new_date())
  expect_identical(vec_ptype2(new_date(), new_ym()), new_date())
})

test_that("ym - POSIXct", {
  datetime <- new_datetime(tzone = "America/New_York")
  expect_identical(vec_ptype2(new_ym(), datetime), datetime)
  expect_identical(vec_ptype2(datetime, new_ym()), datetime)
})

test_that("ym - POSIXlt", {
  datetime_ct <- new_datetime(tzone = "America/New_York")
  datetime_lt <- as.POSIXlt(datetime_ct)
  expect_identical(vec_ptype2(new_ym(), datetime_lt), datetime_ct)
  expect_identical(vec_ptype2(datetime_lt, new_ym()), datetime_ct)
})
