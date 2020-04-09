# ------------------------------------------------------------------------------
# Base

# Only implemented to have better handling of extra indices passed in `...`
#' @export
`[.ymon` <- function(x, i, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  i <- maybe_missing(i, TRUE)

  vec_slice(x, i)
}

#' @export
weekdays.ymon <- function(x, abbreviate = FALSE) {
  weekdays(as.Date(x), abbreviate = abbreviate)
}

#' @export
months.ymon <- function(x, abbreviate = FALSE) {
  months(as.Date(x), abbreviate = abbreviate)
}

#' @export
quarters.ymon <- function(x, ...) {
  out <- (months_to_month(x) - 1L) %/% 3L
  out <- paste0("Q", out + 1L)
  out[is.na(x)] <- NA_character_
  out
}

#' @export
julian.ymon <- function(x, origin = ymon(1970, 1), ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  if (length(origin) != 1L || !is_ymon(origin) || is.na(origin)) {
    abort("`origin` must be a length 1 ymon.")
  }

  origin_days <- months_to_days(origin)
  x_days <- months_to_days(x)

  out <- unclass(x_days) - unclass(origin_days)
  structure(out, origin = origin)
}

# ------------------------------------------------------------------------------
# lubridate

# Registered in .onLoad()
tz.ymon <- function(x) {
  "UTC"
}

# ------------------------------------------------------------------------------
# vctrs

# Slightly faster than `vec_proxy.vctrs_vctr` since we know it isn't a list
#' @export
vec_proxy.ymon <- function(x, ...) {
  x
}
