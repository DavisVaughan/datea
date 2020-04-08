#' Construct a new year month
#'
#' @description
#' `ym()` is a high level constructor for a year month object. It accepts
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
#' A ym object.
#'
#' @export
#' @examples
#' ym(2019, 2)
#' ym(2019, c(11, 12))
ym <- function(year, month) {
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

  new_ym(month)
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
#' `new_ym()` is a fast constructor of ym objects, with minimal checking.
#' A ym object is constructed from the integer number of months since the Unix
#' epoch of 1970-01-01. For more robust creation of ym objects, see [ym()] or
#' [as_ym()].
#'
#' @param x `[integer]`
#'
#'   The number of months since `1970-01-01`.
#'
#' @return
#' A ym object.
#'
#' @export
#' @examples
#' # Internally stored as the number of months since 1970-01-01
#' new_ym(0L)
#' new_ym(100L)
new_ym <- function(x = integer()) {
  # `vec_assert()` is slow, avoid unless we need it for nice error messages
  if (!is_integer(x)) {
    vec_assert(x, integer())
  }

  out <- new_vctr(x, class = "ym", inherit_base_type = FALSE)

  out
}

# ------------------------------------------------------------------------------

#' Is `x` a ym?
#'
#' Test if `x` is a ym object.
#'
#' @param x `[vector]`
#'
#'   An object.
#'
#' @return
#' A logical of length 1. `TRUE` if `x` is a ym, otherwise `FALSE`.
#'
#' @export
#' @examples
#' is_ym(new_ym(1L))
#' is_ym(1)
is_ym <- function(x) {
  inherits(x, "ym")
}
