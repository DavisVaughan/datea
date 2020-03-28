#' @export
format.ym <- function(x, ...) {
  result <- months_to_year_month(x)
  year <- result[[1]]
  month <- result[[2]]

  negative <- year < 0
  out_year <- formatC(abs(year), width = 4, flag = "0")
  out_year[negative] <- paste0("-", out_year[negative])

  out_month <- formatC(month, width = 2, flag = "0")

  out <- paste0(out_year, "-", out_month)

  # Don't use `NA_character_`, as obj_print_data.default() will use
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
