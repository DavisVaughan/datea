#' vctrs compatibility functions
#'
#' These functions are the extensions that allow datea objects to
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
#' @method vec_ptype2 ymon
#' @export vec_ptype2.ymon
vec_ptype2.ymon <- function(x, y, ...) {
  UseMethod("vec_ptype2.ymon", y)
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ymon ymon
#' @export
vec_ptype2.ymon.ymon <- function(x, y, ..., x_arg = "", y_arg = "") {
  datea_global_empty_ymon
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ymon Date
#' @export
vec_ptype2.ymon.Date <- function(x, y, ..., x_arg = "", y_arg = "") {
  datea_global_empty_date
}

#' @method vec_ptype2.Date ymon
#' @export
vec_ptype2.Date.ymon <- function(x, y, ..., x_arg = "", y_arg = "") {
  datea_global_empty_date
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ymon POSIXct
#' @export
vec_ptype2.ymon.POSIXct <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXct ymon
#' @export
vec_ptype2.POSIXct.ymon <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(x))
}

# ------------------------------------------------------------------------------

#' @method vec_ptype2.ymon POSIXlt
#' @export
vec_ptype2.ymon.POSIXlt <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(y))
}

#' @method vec_ptype2.POSIXlt ymon
#' @export
vec_ptype2.POSIXlt.ymon <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_datetime(tzone = tzone(x))
}

