#' Parse a character vector as year month
#'
#' @description
#' `ym_parse()` is a parser for turning character input into ym objects. The
#' character method for `as_ym()` is very strict, and only parses input of the
#' form `"YYYY-MM"`, erroring on any failures. `ym_parse()` is more flexible
#' and will only issue a warning if parsing fails.
#'
#' Internally:
#'
#' - `"-%d"` is appended to the `format`, and `"-01"` is appended to `x`.
#'
#' - An attempt to parse as a Date is then made.
#'
#' - The resulting Date is converted to ym, with a warning if any input failed
#'   to parse. Failures result in `NA`.
#'
#' @param x `[character]`
#'
#'   A character vector to coerce to ym.
#'
#' @param format `[character(1)]`
#'
#'   A format to parse character input with. Should generally only consist
#'   of format tokens related to year or month. Common formats are `"%Y-%m"`,
#'   `"%b %Y"`, and `"%Y %b"`.
#'
#' @export
#' @examples
#' ym_parse("1970-01")
#' ym_parse("1970 Jan", format = "%Y %b")
#'
#' # Unparseable input results in `NA`, with a warning
#' try(ym_parse(c("1970-00", "1970-01")))
ym_parse <- function(x, format = "%Y-%m") {
  if (!is_character(x)) {
    abort("`x` must be a character vector.")
  }

  if (!is_character(format) || length(format) != 1L) {
    abort("`format` must be a string.")
  }

  # Avoid bad behavior of `paste()` with zero length input
  if (length(x) == 0L) {
    out <- new_ym()
    names(out) <- names(x)
    return(out)
  }

  # Unambiguously append a `-01`, assuming that the user has provided ONLY
  # the year and month in some format
  format <- paste0(format, "-%d")
  out <- paste0(x, "-01")

  out <- as.Date(out, format = format, origin = timeclass_global_origin_date)
  out <- force_to_ym_from_date(out)

  new_na_detected <- is.na(out) & !is.na(x)
  if (any(new_na_detected)) {
    locations <- which(new_na_detected)
    warn_lossy_parse(locations)
  }

  names(out) <- names(x)

  out
}

warn_lossy_parse <- function(locations) {
  if (length(locations) > 5) {
    locations <- c(locations[1:5], "etc.")
    full_stop <- ""
  } else {
    full_stop <- "."
  }

  if (length(locations) == 1L) {
    chr_location <- "location"
    chr_where <- "that location"
  } else {
    chr_location <- "locations"
    chr_where <- "those locations"
  }

  locations <- paste0(locations, collapse = ", ")

  message <- paste0(
    "Unable to parse to year month at ",
    chr_location,
    " ",
    locations,
    full_stop,
    " ",
    "Returning `NA` at ",
    chr_where,
    "."
  )

  warn(message)
}
