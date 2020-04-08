.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("lubridate::tz", "ym")
}
