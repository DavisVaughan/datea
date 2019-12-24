#' @export
ym <- function(x = integer()) {
  x <- vec_cast(x, integer())
  size <- length(x)

  if (size == 0L) {
    return(new_ym())
  }

  # https://github.com/tidyverse/lubridate/issues/846
  if (size == 1L && is.na(x)) {
    return(new_ym(NA_real_))
  }

  out <- months_to_days(x)

  new_ym(out)
}

# x = Number of days since 1970-01-01
# Only allowed values should be month starts
# Internally stored as double for compat with most Date operations

new_ym <- function(x = double()) {
  vec_assert(x, ptype = double())

  out <- new_vctr(x, class = "ym", inherit_base_type = FALSE)
  class(out) <- c(class(out), "Date")

  out
}
