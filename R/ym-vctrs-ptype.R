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

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym ym
#' @export
vec_ptype2.ym.ym <- function(x, y, ..., x_arg = "", y_arg = "") {
  timeclass_global_empty_ym
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym Date
#' @export
vec_ptype2.ym.Date <- function(x, y, ..., x_arg = "", y_arg = "") {
  timeclass_global_empty_date
}

#' @method vec_ptype2.Date ym
#' @export
vec_ptype2.Date.ym <- function(x, y, ..., x_arg = "", y_arg = "") {
  timeclass_global_empty_date
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym POSIXct
#' @export
vec_ptype2.ym.POSIXct <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXct ym
#' @export
vec_ptype2.POSIXct.ym <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(x))
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ym POSIXlt
#' @export
vec_ptype2.ym.POSIXlt <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXlt ym
#' @export
vec_ptype2.POSIXlt.ym <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(x))
}

