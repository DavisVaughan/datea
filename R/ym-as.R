# TODO:
# as_ym.integer()
# as_ym.double()
# as_ym.character()

#' Coerce to year month
#'
#' @description
#' Coerce a vector to ym. Supports a range of types including:
#'
#' - Date
#' - POSIXct
#' - POSIXlt
#'
#' @param x `[vector]`
#'
#'   An object to coerce to ym.
#'
#' @param ...
#'
#'   Not used.
#'
#' @export
#' @examples
#' # Extra information such as days, hours, or time zones are dropped
#' as_ym(as.Date("2019-05-03"))
#' as_ym(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
as_ym <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  UseMethod("as_ym")
}

#' @export
as_ym.default <- function(x, ...) {
  class <- class_collapse(x)
  abort(paste0("Can't convert a <", class, "> to a <ym>."))
}

#' @export
as_ym.ym <- function(x, ...) {
  x
}

#' @export
as_ym.Date <- function(x, ...) {
  force_to_ym_from_date(x)
}

force_to_ym_from_date <- function(x) {
  out <- warp_distance(x, period = "month")
  out <- as.integer(out)
  out <- new_ym(out)
  names(out) <- names(x)
  out
}

#' @export
as_ym.POSIXct <- function(x, ...) {
  force_to_ym_from_posixct(x)
}

force_to_ym_from_posixct <- function(x) {
  force_to_ym_from_posixt(x)
}

force_to_ym_from_posixt <- function(x) {
  # Drop time zone to avoid any DST weirdness in the
  # `warp_distance(period = "month")` call
  out <- as.Date(x)
  out <- force_to_ym_from_date(out)
  out
}

#' @export
as_ym.POSIXlt <- function(x, ...) {
  force_to_ym_from_posixlt(x)
}

force_to_ym_from_posixlt <- function(x) {
  out <- force_to_ym_from_posixt(x)

  # `as.Date.POSIXlt()` used in `force_to_ym_from_posixt()`
  # doesn't retain names! Bug!
  names(out) <- names(x)

  out
}

# ------------------------------------------------------------------------------

#' @export
as.Date.ym <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_date_from_ym(x)
}

force_to_date_from_ym <- function(x) {
  out <- months_to_days(x)
  out <- new_date(out)
  names(out) <- names(x)
  out
}

# ------------------------------------------------------------------------------

#' @export
as.POSIXct.ym <- function(x, tz = "UTC", ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_posixct_from_ym(x, tz)
}

force_to_posixct_from_ym <- function(x, tz) {
  force_to_posixt_from_ym(x, tz, posixct = TRUE)
}

force_to_posixt_from_ym <- function(x, tz, posixct) {
  out <- force_to_date_from_ym(x)
  out <- as.character(out)

  if (posixct) {
    out <- as.POSIXct(out, tz = tz)
  } else {
    out <- as.POSIXlt(out, tz = tz)
  }

  out
}

# ------------------------------------------------------------------------------

#' @export
as.POSIXlt.ym <- function(x, tz = "UTC", ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_posixlt_from_ym(x, tz)
}

force_to_posixlt_from_ym <- function(x, tz) {
  force_to_posixt_from_ym(x, tz, posixct = FALSE)
}
