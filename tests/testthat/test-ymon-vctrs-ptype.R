test_that("default method", {
  expect_error(vec_ptype2(new_ymon(), 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(1, new_ymon()), class = "vctrs_error_incompatible_type")
})

test_that("unspecified method", {
  expect_identical(vec_ptype2(new_ymon(), NA), new_ymon())
  expect_identical(vec_ptype2(NA, new_ymon()), new_ymon())
})

test_that("ymon - ymon", {
  expect_identical(vec_ptype2(new_ymon(), new_ymon()), new_ymon())
})

test_that("ymon - Date", {
  expect_identical(vec_ptype2(new_ymon(), new_date()), new_date())
  expect_identical(vec_ptype2(new_date(), new_ymon()), new_date())
})

test_that("ymon - POSIXct", {
  datetime <- new_datetime(tzone = "America/New_York")
  expect_identical(vec_ptype2(new_ymon(), datetime), datetime)
  expect_identical(vec_ptype2(datetime, new_ymon()), datetime)
})

test_that("ymon - POSIXlt", {
  datetime_ct <- new_datetime(tzone = "America/New_York")
  datetime_lt <- as.POSIXlt(datetime_ct)
  expect_identical(vec_ptype2(new_ymon(), datetime_lt), datetime_ct)
  expect_identical(vec_ptype2(datetime_lt, new_ymon()), datetime_ct)
})
