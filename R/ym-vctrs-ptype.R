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
#' @method vec_ptype2 ym
#' @export vec_ptype2.ym
vec_ptype2.ym <- function(x, y, ...) {
  UseMethod("vec_ptype2.ym", y)
}

#' @method vec_ptype2.ym default
#' @export
vec_ptype2.ym.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym ym
#' @export
vec_ptype2.ym.ym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_ym()
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym Date
#' @export
vec_ptype2.ym.Date <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_date()
}

#' @method vec_ptype2.Date ym
#' @export
vec_ptype2.Date.ym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_date()
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym POSIXt
#' @export
vec_ptype2.ym.POSIXt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXt ym
#' @export
vec_ptype2.POSIXt.ym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_datetime(tzone = tzone(x))
}


