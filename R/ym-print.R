#' @export
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
