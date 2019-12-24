#' @export
as_ym <- function(x, ...) {
  UseMethod("as_ym")
}

#' @export
as_ym.ym <- function(x, ...) {
  x
}

#' @export
as_ym.Date <- function(x, ...) {
  vec_cast(x, new_ym())
}

#' @export
as_ym.POSIXt <- function(x, ...) {
  vec_cast(x, new_ym())
}

#' @export
as_ym.numeric <- function(x, ...) {
  vec_cast(x, new_ym())
}
