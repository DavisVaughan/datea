#' Construct a new year month
#'
#' `ym()` is a high level constructor for a year month object. It accepts
#' integer year and month components to build the object from.
#'
#' @param year `[integer / NULL]`
#'
#'   The year value. If left as `NULL` and any `month` values are present,
#'   a default of `0` is used.
#'
#' @param month `[integer / NULL]`
#'
#'   The month value, in the range of `1-12`. If left as `NULL` and any `year`
#'   values are present, a default of `1` is used.
#'
#' @return
#' A ym object made from `year-month`.
#'
#' @export
#' @examples
#' ym()
#'
#' ym(2019)
#'
#' ym(2019, 2)
#'
#' ym(month = 2)
ym <- function(year = NULL, month = NULL) {
  missing_year <- is.null(year)
  missing_month <- is.null(month)

  if (missing_year && missing_month) {
    return(new_ym())
  }

  year <- vec_cast(year, integer(), x_arg = "year")
  month <- vec_cast(month, integer(), x_arg = "month")

  if (any_oob_month(month)) {
    abort("`month` values must be between `1` and `12`.")
  }

  # Month becomes 0-based for counting purposes
  if (!missing_month) {
    month <- month - 1L
  }

  if (missing_month) {
    month <- years_to_months(year)
    day <- months_to_days(month)
    return(new_ym(day))
  }

  if (missing_year) {
    month <- month + years_to_months(0L)
    day <- months_to_days(month)
    return(new_ym(day))
  }

  n_year <- length(year)
  n_month <- length(month)

  if (n_year != n_month) {
    abort(paste0(
      "The length of `year` (", n_year, ") must be equal to ",
      "the length of `month` (", n_month, ")."
    ))
  }

  month <- month + years_to_months(year)
  day <- months_to_days(month)

  new_ym(day)
}

years_to_months <- function(year) {
  (year - 1970L) * 12L
}

any_oob_month <- function(month) {
  if (is.null(month)) {
    return(FALSE)
  }

  any(month > 12L | month < 1L, na.rm = TRUE)
}

# ------------------------------------------------------------------------------

#' Construct a new year month
#'
#' @description
#' `new_ym()` is a fast constructor of ym objects, with minimal checking.
#' It is intended that `x` is restricted to only values that correspond to
#' the first day of the month, but importantly this is not checked here. For
#' more robust creation of ym objects, see [ym()] or [as_ym()].
#'
#' @details
#' `new_ym()` is suitable for use as a prototype for the ym class.
#'
#' Internally, a ym is stored as the number of days since `"1970-01-01"`, the
#' same as the Date class. It is stored as a double rather than an integer,
#' since most of the time this is what Date does as well.
#'
#' A ym inherits from `"Date"` as its highest superclass, but also inherits
#' from the class `"vctrs_vctr"` to gain the well-thought-out vctrs methods.
#' There are a number of benefits gained from inheriting from `"Date"`. One of
#' which is that functions that dispatch on `"Date"` will work automatically,
#' like `lubridate::year()`.
#'
#' @param x `[double]`
#'
#'   The number of months since `1970-01-01`, passed in as the number of days.
#'
#' @return
#' A ym object.
#'
#' @export
#' @examples
#' # Internally stored as the number of days since 1970-01-01
#' new_ym(0)
#' new_ym(31)
#'
#' # But this is not enforced with this low-level constructor,
#' # so technically you can create ym objects with invalid entries
#' new_ym(2)
#' as.Date(new_ym(2))
new_ym <- function(x = double()) {
  vec_assert(x, ptype = double())

  out <- new_vctr(x, class = "ym", inherit_base_type = FALSE)
  class(out) <- c(class(out), "Date")

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
#' is_ym(ym(1))
#'
#' is_ym(1)
is_ym <- function(x) {
  inherits(x, "ym")
}

# ------------------------------------------------------------------------------

# We could leave this to `is.numeric.Date()` but I'd rather not

#' @export
is.numeric.ym <- function(x) {
  FALSE
}
