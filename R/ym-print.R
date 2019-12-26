#' @export
format.ym <- function(x, ...) {
  # Avoid going through vec_cast.POSIXlt.ym() which goes through
  # vec_cast.POSIXlt.Date(). This uses as.character() on the date
  # then converts to POSIXlt to retain wall time, but strptime() only
  # works with years in the 0-9999 range.
  components <- vec_cast(x, new_date())
  components <- as.POSIXlt(components)

  year <- components$year + 1900L
  month <- components$mon + 1L

  year <- formatC(year, width = 4, flag = "0")
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
