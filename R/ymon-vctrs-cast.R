# TODO: Switch usage of `maybe_lossy_cast()` to immediate calling of
# `stop_lossy_cast()`, plus lazy error generation. In the short term this will
# double the work, because the lossy locations will have to be detected twice,
# but in the long term if the cast methods move to C then this will be a good
# thing. Waiting on: https://github.com/r-lib/vctrs/issues/978

#' @export
#' @rdname vctrs-compat
#' @method vec_cast ymon
#' @export vec_cast.ymon
vec_cast.ymon <- function(x, to, ...) {
  UseMethod("vec_cast.ymon")
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ymon ymon
#' @export
vec_cast.ymon.ymon <- function(x, to, ..., x_arg = "", to_arg = "") {
  x
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ymon Date
#' @export
vec_cast.ymon.Date <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_ymon_from_date(x, to, x_arg, to_arg)
}

#' @method vec_cast.Date ymon
#' @export
vec_cast.Date.ymon <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_date_from_ymon(x, to, x_arg, to_arg)
}

cast_to_ymon_from_date <- function(x, to, x_arg, to_arg) {
  out <- force_to_ymon_from_date(x)

  # Check lossy cast
  date <- force_to_date_from_ymon(out)
  lossy <- compute_lossy(date, x)

  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

# Never lossy
cast_to_date_from_ymon <- function(x, to, x_arg, to_arg) {
  force_to_date_from_ymon(x)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ymon POSIXct
#' @export
vec_cast.ymon.POSIXct <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_ymon_from_posixct(x, to, x_arg, to_arg)
}

#' @method vec_cast.POSIXct ymon
#' @export
vec_cast.POSIXct.ymon <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_posixct_from_ymon(x, to, x_arg, to_arg)
}

cast_to_ymon_from_posixct <- function(x, to, x_arg, to_arg) {
  out <- force_to_ymon_from_posixct(x)

  datetime <- force_to_posixct_from_ymon(out, tz = tzone(x))
  lossy <- compute_lossy(datetime, x)

  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

# Never lossy
cast_to_posixct_from_ymon <- function(x, to, x_arg, to_arg) {
  force_to_posixct_from_ymon(x, tz = tzone(to))
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ymon POSIXlt
#' @export
vec_cast.ymon.POSIXlt <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_ymon_from_posixlt(x, to, x_arg, to_arg)
}

#' @method vec_cast.POSIXlt ymon
#' @export
vec_cast.POSIXlt.ymon <- function(x, to, ..., x_arg = "", to_arg = "") {
  cast_to_posixlt_from_ymon(x, to, x_arg, to_arg)
}

cast_to_ymon_from_posixlt <- function(x, to, x_arg, to_arg) {
  out <- force_to_ymon_from_posixlt(x)

  x_ct <- as.POSIXct(x)
  datetime <- force_to_posixct_from_ymon(out, tz = tzone(x_ct))
  lossy <- compute_lossy(datetime, x_ct)

  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

cast_to_posixlt_from_ymon <- function(x, to, x_arg, to_arg) {
  force_to_posixlt_from_ymon(x, tz = tzone(to))
}

# ------------------------------------------------------------------------------

compute_lossy <- function(new, old) {
  abs(unclass(new) - unclass(old)) > 1e-09 & !is.na(old)
}
