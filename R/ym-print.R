#' @export
format.ym <- function(x, format = "%Y-%m", ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  x_lt <- as.POSIXlt(x)

  out <- format(x_lt, format = format, usetz = FALSE)

  # Don't use `NA_character_`, as `obj_print_data.default()` will use
  # `print(quote = FALSE)` which prints it as `<NA>`
  out[is.na(x)] <- "NA"

  out
}

#' @export
vec_ptype_abbr.ym <- function(x, ...) {
  "ym"
}

#' @export
vec_ptype_full.ym <- function(x, ...) {
  "ym"
}
