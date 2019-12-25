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
  switch(
    op,
    "-" = ym_ym_minus(x, y),
    stop_incompatible_op(op, x, y)
  )
}

ym_ym_minus <- function(x, y) {
  vec_cast(x, integer()) - vec_cast(y, integer())
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

  y <- vec_cast(y, integer())

  switch(
    op,
    "-" = ym_integer_minus(x, y),
    "+" = ym_integer_plus(x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.numeric ym
#' @export
vec_arith.numeric.ym <- function(op, x, y, ...) {
  if (!is_one_dim(y)) {
    stop_incompatible_op(op, x, y)
  }

  x <- vec_cast(x, integer())

  switch(
    op,
    "+" = ym_integer_plus(y, x),
    stop_incompatible_op(op, x, y)
  )
}

ym_integer_minus <- function(x, y) {
  months <- vec_cast(x, integer()) - y
  ym(months)
}

ym_integer_plus <- function(x, y) {
  months <- vec_cast(x, integer()) + y
  ym(months)
}
