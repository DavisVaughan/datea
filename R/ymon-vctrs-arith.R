#' @export
#' @rdname vctrs-compat
#' @method vec_arith ymon
#' @export vec_arith.ymon
vec_arith.ymon <- function(op, x, y, ...) {
  UseMethod("vec_arith.ymon", y)
}

#' @method vec_arith.ymon default
#' @export
vec_arith.ymon.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ymon ymon
#' @export
vec_arith.ymon.ymon <- function(op, x, y, ...) {
  check_tidy_recyclable(x, y)

  switch(
    op,
    "-" = minus_ymon_ymon(x, y),
    stop_incompatible_op(op, x, y)
  )
}

# Return the number of months between `x` and `y`
minus_ymon_ymon <- function(x, y) {
  unclass(x) - unclass(y)
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ymon MISSING
#' @export
vec_arith.ymon.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "+" = x,
    stop_incompatible_op(op, x, y)
  )
}

# ------------------------------------------------------------------------------

#' @method vec_arith.ymon numeric
#' @export
vec_arith.ymon.numeric <- function(op, x, y, ...) {
  if (!is_one_dim(y)) {
    stop_incompatible_op(op, x, y)
  }

  if (is_double(y)) {
    y <- vec_cast_integer(y)
  }

  check_tidy_recyclable(x, y)

  switch(
    op,
    "-" = minus_ymon_integer(x, y),
    "+" = plus_ymon_integer(x, y),
    stop_incompatible_op(op, x, y)
  )
}

minus_ymon_integer <- function(x, y) {
  x <- unclass(x)
  out <- x - y
  new_ymon(out)
}

plus_ymon_integer <- function(x, y) {
  x <- unclass(x)
  out <- x + y
  new_ymon(out)
}

#' @method vec_arith.numeric ymon
#' @export
vec_arith.numeric.ymon <- function(op, x, y, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_op(op, x, y)
  }

  if (is_double(x)) {
    x <- vec_cast_integer(x)
  }

  check_tidy_recyclable(x, y)

  switch(
    op,
    "+" = plus_integer_ymon(x, y),
    stop_incompatible_op(op, x, y)
  )
}

plus_integer_ymon <- function(x, y) {
  y <- unclass(y)
  out <- x + y
  new_ymon(out)
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
