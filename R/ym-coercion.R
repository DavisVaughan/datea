#' @export
as_ym <- function(x, ...) {
  UseMethod("as_ym")
}

#' @export
as_ym.default <- function(x, ...) {
  class <- class_collapse(x)
  abort(paste0("Can't coerce a `", class, "` to a `ym`."))
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

# ------------------------------------------------------------------------------

# TODO - Remove after:
# https://github.com/r-lib/vctrs/issues/717

#' @export
as.POSIXlt.vctrs_vctr <- function(x, tz = "", ...) {
  to <- as.POSIXlt(new_datetime(), tz = tz)
  vec_cast(x, to)
}
