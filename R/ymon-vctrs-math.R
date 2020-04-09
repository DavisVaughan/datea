#' @export
vec_math.ymon <- function(.fn, .x, ...) {
  .fn <- switch(.fn,
    "is.nan" = is_nan_ymon,
    "is.finite" = is_finite_ymon,
    "is.infinite" = is_infinite_ymon,
    glubort("`{.fn}()` is not supported for <ymon>.")
  )

  .fn(.x, ...)
}

is_nan_ymon <- function(x) {
  vector("logical", vec_size(x))
}

is_finite_ymon <- function(x) {
  !vec_equal_na(x)
}

is_infinite_ymon <- function(x) {
  vector("logical", vec_size(x))
}
