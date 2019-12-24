#' @export
ym <- function(x = integer()) {
  x <- vec_cast(x, integer())
  size <- length(x)

  out <- months_to_days(x)

  new_ym(out)
}

# ------------------------------------------------------------------------------

#' @export
is_ym <- function(x) {
  inherits(x, "ym")
}

# ------------------------------------------------------------------------------

# x = Number of days since 1970-01-01
# Only allowed values should be month starts

# Internally stored as double for compat with most Date operations,
# but `ym()` forces input to be integer first to avoid fractional days

# Exported, but it should be assumed that no checks are made for validity!

#' @export
new_ym <- function(x = double()) {
  vec_assert(x, ptype = double())

  out <- new_vctr(x, class = "ym", inherit_base_type = FALSE)
  class(out) <- c(class(out), "Date")

  out
}
