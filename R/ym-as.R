#' Coerce to year month
#'
#' @description
#' Coerce a vector to ym. Supports a range of types including:
#'
#' - Date
#' - POSIXct
#' - POSIXlt
#'
#' @param x `[vector]`
#'
#'   An object to coerce to ym.
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
as_ym <- function(x, ...) {
  UseMethod("as_ym")
}

#' @export
as_ym.default <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()

  class <- class_collapse(x)
  abort(paste0("Can't convert a <", class, "> to a <ym>."))
}

#' @export
as_ym.ym <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()
  x
}

#' @export
as_ym.Date <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()
  force_to_ym_from_date(x)
}

force_to_ym_from_date <- function(x) {
  out <- warp_distance(x, period = "month")
  out <- as.integer(out)
  out <- new_ym(out)
  names(out) <- names(x)
  out
}

#' @export
as_ym.POSIXct <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()
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

#' @export
as_ym.POSIXlt <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()
  force_to_ym_from_posixlt(x)
}

force_to_ym_from_posixlt <- function(x) {
  out <- force_to_ym_from_posixt(x)

  # `as.Date.POSIXlt()` used in `force_to_ym_from_posixt()`
  # doesn't retain names! Bug!
  names(out) <- names(x)

  out
}

#' @export
as_ym.integer <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()
  new_ym(x)
}

#' @export
as_ym.double <- function(x, ...) {
  if (!missing(...)) ellipsis::check_dots_empty()

  out <- vec_cast(x, integer())
  out <- new_ym(out)

  # `vec_cast()` currently doesn't always keep names
  names(out) <- names(x)

  out
}

#' @export
as_ym.character <- function(x, format = "%Y-%m", ...) {
  if (!missing(...)) ellipsis::check_dots_empty()

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
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

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
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

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
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_posixlt_from_ym(x, tz)
}

force_to_posixlt_from_ym <- function(x, tz) {
  force_to_posixt_from_ym(x, tz, posixct = FALSE)
}

# ------------------------------------------------------------------------------

#' @export
as.character.ym <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

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
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_integer_from_ym(x)
}

force_to_integer_from_ym <- function(x) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @export
as.double.ym <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  force_to_double_from_ym(x)
}

force_to_double_from_ym <- function(x) {
  out <- force_to_integer_from_ym(x)
  # `as.double()` purposefully drops names
  out <- as.double(out)
  names(out) <- names(x)
  out
}
