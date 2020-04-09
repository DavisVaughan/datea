# ------------------------------------------------------------------------------
# Base

# Only implemented to have better handling of extra indices passed in `...`
#' @export
`[.ym` <- function(x, i, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  i <- maybe_missing(i, TRUE)

  vec_slice(x, i)
}

#' @export
weekdays.ym <- function(x, abbreviate = FALSE) {
  weekdays(as.Date(x), abbreviate = abbreviate)
}

#' @export
months.ym <- function(x, abbreviate = FALSE) {
  months(as.Date(x), abbreviate = abbreviate)
}

#' @export
quarters.ym <- function(x) {
  out <- (months_to_month(x) - 1L) %/% 3L
  out <- paste0("Q", out + 1L)
  out[is.na(x)] <- NA_character_
  out
}

#' @export
julian.ym <- function(x, origin = ym(1970, 1), ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  if (length(origin) != 1L || !is_ym(origin) || is.na(origin)) {
    abort("`origin` must be a length 1 ym.")
  }

  origin_days <- months_to_days(origin)
  x_days <- months_to_days(x)

  out <- unclass(x_days) - unclass(origin_days)
  structure(out, origin = origin)
}

# ------------------------------------------------------------------------------
# lubridate

# Registered in .onLoad()
tz.ym <- function(x) {
  "UTC"
}

# ------------------------------------------------------------------------------
# vctrs

# Slightly faster than `vec_proxy.vctrs_vctr` since we know it isn't a list
#' @export
vec_proxy.ym <- function(x, ...) {
  x
}
