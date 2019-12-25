#' @export
ym <- function(x = integer()) {
  x <- vec_cast(x, integer())

  out <- months_to_days(x)

  new_ym(out)
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
