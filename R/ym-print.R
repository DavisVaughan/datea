#' Format a ym
#'
#' The ym `format()` method is purposefully implemented without a `format =`
#' argument, and always prints as `YYYY-MM`. To use a custom format, the ym
#' would have to be converted to POSIXlt, which is a very slow operation. It is
#' also buggy when outside the year range of 0-9999 because strptime doesn't
#' support it. If you want to use a custom format, convert to Date first with
#' [as.Date()], and then format that.
#'
#' @param x A ym.
#' @param ... Unused.
#'
#' @export
#'
#' @examples
#' format(ym(2019, 01))
#'
#' # NA values are printed as the actual character string
#' # for better behavior when combined with `print(format(x), quote = FALSE)`.
#' format(ym(NA, NA))
format.ym <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  x <- force_to_character_from_ym(x)

  # Don't use `NA_character_`, as `obj_print_data.default()` will use
  # `print(quote = FALSE)` which prints it as `<NA>`
  if (anyNA(x)) {
    x[is.na(x)] <- "NA"
  }

  x
}

#' @export
vec_ptype_abbr.ym <- function(x, ...) {
  "ym"
}

#' @export
vec_ptype_full.ym <- function(x, ...) {
  "ym"
}
