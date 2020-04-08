#' @export
vec_math.ym <- function(.fn, .x, ...) {
  .fn <- switch(.fn,
    "is.nan" = is_nan_ym,
    "is.finite" = is_finite_ym,
    "is.infinite" = is_infinite_ym,
    glubort("`{.fn}()` is not supported for <ym>.")
  )

  .fn(.x, ...)
}

is_nan_ym <- function(x) {
  vector("logical", vec_size(x))
}

is_finite_ym <- function(x) {
  !vec_equal_na(x)
}

is_infinite_ym <- function(x) {
  vector("logical", vec_size(x))
}
