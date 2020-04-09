#' @export
#' @rdname vctrs-compat
#' @method vec_arith ym
#' @export vec_arith.ym
vec_arith.ym <- function(op, x, y, ...) {
  UseMethod("vec_arith.ym", y)
}

#' @method vec_arith.ym default
#' @export
vec_arith.ym.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ym ym
#' @export
vec_arith.ym.ym <- function(op, x, y, ...) {
  check_tidy_recyclable(x, y)

  switch(
    op,
    "-" = minus_ym_ym(x, y),
    stop_incompatible_op(op, x, y)
  )
}

# Return the number of months between `x` and `y`
minus_ym_ym <- function(x, y) {
  unclass(x) - unclass(y)
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ym MISSING
#' @export
vec_arith.ym.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "+" = x,
    stop_incompatible_op(op, x, y)
  )
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ym numeric
#' @export
vec_arith.ym.numeric <- function(op, x, y, ...) {
  if (!is_one_dim(y)) {
    stop_incompatible_op(op, x, y)
  }

  if (is_double(y)) {
    y <- vec_cast_integer(y)
  }

  check_tidy_recyclable(x, y)

  switch(
    op,
    "-" = minus_ym_integer(x, y),
    "+" = plus_ym_integer(x, y),
    stop_incompatible_op(op, x, y)
  )
}

minus_ym_integer <- function(x, y) {
  x <- unclass(x)
  out <- x - y
  new_ym(out)
}

plus_ym_integer <- function(x, y) {
  x <- unclass(x)
  out <- x + y
  new_ym(out)
}

#' @method vec_arith.numeric ym
#' @export
vec_arith.numeric.ym <- function(op, x, y, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_op(op, x, y)
  }

  if (is_double(x)) {
    x <- vec_cast_integer(x)
  }

  check_tidy_recyclable(x, y)

  switch(
    op,
    "+" = plus_integer_ym(x, y),
    stop_incompatible_op(op, x, y)
  )
}

plus_integer_ym <- function(x, y) {
  y <- unclass(y)
  out <- x + y
  new_ym(out)
}

# ------------------------------------------------------------------------------

# `vec_cast()` currently doesn't retain names
# https://github.com/r-lib/vctrs/issues/623
vec_cast_integer <- function(x) {
  out <- vec_cast(x, integer())
  names(out) <- names(x)
  out
}

# Used solely to error if not recyclable. Relying on base R's
# efficient arithmetic recycling if they are tidy recyclable.
# Would be better off using `vec_size2()` here to get a good error
# https://github.com/r-lib/vctrs/issues/1000
check_tidy_recyclable <- function(x, y) {
  vec_size_common(x, y)
  invisible()
}
