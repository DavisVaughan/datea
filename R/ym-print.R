#' @export
format.ym <- function(x, ...) {
  x_lt <- as.POSIXlt(x)

  year <- x_lt$year + 1900L
  month <- x_lt$mon + 1L

  month <- formatC(month, width = 2, flag = "0")

  out <- paste0(year, "-", month)
  out[is.na(x)] <- NA_character_

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
