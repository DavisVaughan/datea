#' vctrs compatibility functions
#'
#' These functions are the extensions that allow timeclass objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param to Type to cast to.
#' @param op An arithmetic operator as a string.
#' @param ... Used to pass along error message information.
#'
#' @return
#' See the corresponding vctrs function for the exact return value.
#'
#' @name vctrs-compat
#'
NULL

#' @export
#' @rdname vctrs-compat
#' @method vec_ptype2 yearmonth
#' @export vec_ptype2.yearmonth
vec_ptype2.yearmonth <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearmonth", y)
}

#' @method vec_ptype2.yearmonth default
#' @export
vec_ptype2.yearmonth.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.yearmonth yearmonth
#' @export
vec_ptype2.yearmonth.yearmonth <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_yearmonth()
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.yearmonth Date
#' @export
vec_ptype2.yearmonth.Date <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_date()
}

#' @method vec_ptype2.Date yearmonth
#' @export
vec_ptype2.Date.yearmonth <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_date()
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.yearmonth POSIXt
#' @export
vec_ptype2.yearmonth.POSIXt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXt yearmonth
#' @export
vec_ptype2.POSIXt.yearmonth <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_datetime(tzone = tzone(x))
}


