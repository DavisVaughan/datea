#' Construct a new ymon
#'
#' @description
#' `ymon()` is a high level constructor for a ymon object. It accepts
#' integer year and month components to build the object from.
#'
#' @details
#' `year` and `month` are recycled to a common size.
#'
#' @param year `[integer]`
#'
#'   The year value.
#'
#' @param month `[integer]`
#'
#'   The month value, in the range of `1-12`.
#'
#' @return
#' A ymon object.
#'
#' @export
#' @examples
#' ymon(2019, 2)
#' ymon(2019, c(11, 12))
ymon <- function(year, month) {
  year <- vec_cast(year, integer(), x_arg = "year")
  month <- vec_cast(month, integer(), x_arg = "month")

  if (any_oob_month(month)) {
    abort("`month` values must be between `1` and `12`.")
  }

  args <- vec_recycle_common(year, month)
  year <- args[[1]]
  month <- args[[2]]

  # Month becomes 0-based for counting purposes
  month <- month - 1L

  month <- month + years_to_months(year)

  new_ymon(month)
}

years_to_months <- function(year) {
  (year - 1970L) * 12L
}

any_oob_month <- function(month) {
  any(month > 12L | month < 1L, na.rm = TRUE)
}

# ------------------------------------------------------------------------------

#' Construct a new year month
#'
#' @description
#' `new_ymon()` is a fast constructor of ymon objects, with minimal checking.
#' A ymon object is constructed from the integer number of months since the Unix
#' epoch of 1970-01-01. For more robust creation of ymon objects, see [ymon()] or
#' [as_ymon()].
#'
#' @param x `[integer]`
#'
#'   The number of months since `1970-01-01`.
#'
#' @return
#' A ymon object.
#'
#' @export
#' @examples
#' # Internally stored as the number of months since 1970-01-01
#' new_ymon(0L)
#' new_ymon(100L)
new_ymon <- function(x = integer()) {
  # `vec_assert()` is slow, avoid unless we need it for nice error messages
  if (!is_integer(x)) {
    vec_assert(x, integer())
  }

  out <- new_vctr(x, class = "ymon", inherit_base_type = FALSE)

  out
}

# ------------------------------------------------------------------------------

#' Is `x` a ymon?
#'
#' Test if `x` is a ymon object.
#'
#' @param x `[vector]`
#'
#'   An object.
#'
#' @return
#' A logical of length 1. `TRUE` if `x` is a ymon, otherwise `FALSE`.
#'
#' @export
#' @examples
#' is_ymon(new_ymon(1L))
#' is_ymon(1)
is_ymon <- function(x) {
  inherits(x, "ymon")
}

# ------------------------------------------------------------------------------

#' Accessors
#'
#' @description
#' - `ymon_year()` returns the ymon year.
#'
#' - `ymon_month()` returns the ymon month.
#'
#' @param x `[ymon]`
#'
#'   A ymon object.
#'
#' @name ymon-accessors
#'
#' @examples
#' x <- ymon(
#'   year = c(2000, NA, 2001),
#'   month = c(1, 2, 3)
#' )
#'
#' ymon_year(x)
NULL

#' @rdname ymon-accessors
#' @export
ymon_year <- function(x) {
  if (!is_ymon(x)) {
    stop_requires_ymon(x)
  }

  months_to_year(x)
}

#' @rdname ymon-accessors
#' @export
ymon_month <- function(x) {
  if (!is_ymon(x)) {
    stop_requires_ymon(x)
  }

  months_to_month(x)
}
