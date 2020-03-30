#' Coerce to year month
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly by extracting the year
#'   and month from `x`. Any day, hour, minute, or second components are
#'   dropped. Time zone information is not retained.
#'
#' - Integer and double input are assumed to be the number of months since the
#'   Unix origin of 1970-01-01.
#'
#' - Character input is assumed to be provided in a format containing only
#'   information about the year and month, such as `"1970-01"` or `"Jan 1970"`.
#'   The default format is `"%Y-%m"`, but this can be altered.
#'
#' @details
#' When parsing character input, internally:
#'
#' - `"-%d"` is appended to the `format`, and `"-01"` is appended to `x`.
#'
#' - An attempt to parse as a Date is then made.
#'
#' - The resulting Date is converted to ym, with a warning if any input failed
#'   to parse. Failures result in `NA`.
#'
#' @param x `[vector]`
#'
#'   An object to coerce to ym.
#'
#' @param format `[character(1)]`
#'
#'   A format to parse character input with. Should generally only consist
#'   of format tokens related to year or month. Common formats are `"%Y-%m"`,
#'   `"%b %Y"`, and `"%Y %b"`.
#'
#' @param ...
#'
#'   Not used.
#'
#' @export
#' @examples
#' # Extra information such as days, hours, or time zones are dropped
#' as_ym(as.Date("2019-05-03"))
#' as_ym(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#'
#' # Integers are interpreted as the number of months since 1970-01-01
#' as_ym(0L)
#' as_ym(12L)
#'
#' as_ym("1970-01")
#' as_ym("1970 Jan", format = "%Y %b")
#'
#' # Unparseable input results in `NA`, with a warning
#' try(as_ym(c("1970-00", "1970-01")))
as_ym <- function(x, ...) {
  UseMethod("as_ym")
}

#' @export
as_ym.default <- function(x, ...) {
  ellipsis::check_dots_empty()
  class <- class_collapse(x)
  abort(paste0("Can't convert a <", class, "> to a <ym>."))
}

#' @export
as_ym.ym <- function(x, ...) {
  ellipsis::check_dots_empty()
  x
}

#' @rdname as_ym
#' @export
as_ym.Date <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_date(x)
}

force_to_ym_from_date <- function(x) {
  out <- warp_distance(x, period = "month")
  out <- as.integer(out)
  out <- new_ym(out)
  names(out) <- names(x)
  out
}

#' @rdname as_ym
#' @export
as_ym.POSIXct <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_posixct(x)
}

force_to_ym_from_posixct <- function(x) {
  force_to_ym_from_posixt(x)
}

force_to_ym_from_posixt <- function(x) {
  # Drop time zone to avoid any DST weirdness in the
  # `warp_distance(period = "month")` call
  out <- as.Date(x)
  out <- force_to_ym_from_date(out)
  out
}

#' @rdname as_ym
#' @export
as_ym.POSIXlt <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_posixlt(x)
}

force_to_ym_from_posixlt <- function(x) {
  out <- force_to_ym_from_posixt(x)

  # `as.Date.POSIXlt()` used in `force_to_ym_from_posixt()`
  # doesn't retain names! Bug!
  names(out) <- names(x)

  out
}

#' @rdname as_ym
#' @export
as_ym.integer <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_integer(x)
}

force_to_ym_from_integer <- function(x) {
  new_ym(x)
}

#' @rdname as_ym
#' @export
as_ym.double <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_double(x)
}

force_to_ym_from_double <- function(x) {
  out <- vec_cast(x, integer())
  out <- new_ym(out)

  # `vec_cast()` currently doesn't always keep names
  names(out) <- names(x)

  out
}

#' @rdname as_ym
#' @export
as_ym.character <- function(x, format = "%Y-%m", ...) {
  ellipsis::check_dots_empty()
  force_to_ym_from_character(x, format)
}

force_to_ym_from_character <- function(x, format) {
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

# ------------------------------------------------------------------------------

#' @export
as.Date.ym <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_date_from_ym(x)
}

force_to_date_from_ym <- function(x) {
  out <- months_to_days(x)
  out <- as.double(out)
  out <- new_date(out)
  names(out) <- names(x)
  out
}

# ------------------------------------------------------------------------------

#' @export
as.POSIXct.ym <- function(x, tz = "UTC", ...) {
  ellipsis::check_dots_empty()
  force_to_posixct_from_ym(x, tz)
}

force_to_posixct_from_ym <- function(x, tz) {
  force_to_posixt_from_ym(x, tz, posixct = TRUE)
}

force_to_posixt_from_ym <- function(x, tz, posixct) {
  out <- force_to_date_from_ym(x)
  out <- as.character(out)

  if (posixct) {
    out <- as.POSIXct(out, tz = tz)
  } else {
    out <- as.POSIXlt(out, tz = tz)
  }

  out
}

# ------------------------------------------------------------------------------

#' @export
as.POSIXlt.ym <- function(x, tz = "UTC", ...) {
  ellipsis::check_dots_empty()
  force_to_posixlt_from_ym(x, tz)
}

force_to_posixlt_from_ym <- function(x, tz) {
  force_to_posixt_from_ym(x, tz, posixct = FALSE)
}

# ------------------------------------------------------------------------------

#' @export
as.character.ym <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_character_from_ym(x)
}

force_to_character_from_ym <- function(x) {
  # Avoid `formatC(character())` bug with zero-length input
  if (vec_size(x) == 0L) {
    out <- character()
    out <- set_names(out, names(x))
    return(out)
  }

  result <- months_to_year_month(x)
  year <- result[[1]]
  month <- result[[2]]

  negative <- year < 0

  if (any(negative, na.rm = TRUE)) {
    out_year <- formatC(abs(year), width = 4, flag = "0")
    out_year[negative] <- paste0("-", out_year[negative])
  } else {
    out_year <- formatC(year, width = 4, flag = "0")
  }

  out_month <- formatC(month, width = 2, flag = "0")

  out <- paste(out_year, out_month, sep = "-")

  out[is.na(x)] <- NA_character_

  names(out) <- names(x)

  out
}

# ------------------------------------------------------------------------------

#' @export
as.integer.ym <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_integer_from_ym(x)
}

force_to_integer_from_ym <- function(x) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @export
as.double.ym <- function(x, ...) {
  ellipsis::check_dots_empty()
  force_to_double_from_ym(x)
}

force_to_double_from_ym <- function(x) {
  out <- force_to_integer_from_ym(x)
  # `as.double()` purposefully drops names
  out <- as.double(out)
  names(out) <- names(x)
  out
}
