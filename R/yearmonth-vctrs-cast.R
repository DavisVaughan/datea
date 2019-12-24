#' @export
#' @rdname vctrs-compat
#' @method vec_cast yearmonth
#' @export vec_cast.yearmonth
vec_cast.yearmonth <- function(x, to, ...) {
  UseMethod("vec_cast.yearmonth")
}

#' @method vec_cast.yearmonth default
#' @export
vec_cast.yearmonth.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth yearmonth
#' @export
vec_cast.yearmonth.yearmonth <- function(x, to, ...) {
  x
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth Date
#' @export
vec_cast.yearmonth.Date <- function(x, to, ...) {
  months <- warp::warp_distance(x, by = "month")
  yearmonth(months)
}

#' @method vec_cast.Date yearmonth
#' @export
vec_cast.Date.yearmonth <- function(x, to, ...) {
  new_date(vec_data(x))
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth POSIXct
#' @export
vec_cast.yearmonth.POSIXct <- function(x, to, ...) {
  months <- warp::warp_distance(x, by = "month")
  yearmonth(months)
}

#' @method vec_cast.POSIXct yearmonth
#' @export
vec_cast.POSIXct.yearmonth <- function(x, to, ...) {
  x <- vec_cast(x, new_date())
  vec_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth double
#' @export
vec_cast.yearmonth.double <- function(x, to, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_cast(x, to)
  }

  yearmonth(x)
}

#' @method vec_cast.double yearmonth
#' @export
vec_cast.double.yearmonth <- function(x, to, ...) {
  if (!is_one_dim(to)) {
    stop_incompatible_cast(x, to)
  }

  x <- vec_cast(x, new_date())
  warp::warp_distance(x, by = "month")
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth integer
#' @export
vec_cast.yearmonth.integer <- function(x, to, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_cast(x, to)
  }

  yearmonth(x)
}

#' @method vec_cast.integer yearmonth
#' @export
vec_cast.integer.yearmonth <- function(x, to, ...) {
  if (!is_one_dim(to)) {
    stop_incompatible_cast(x, to)
  }

  x <- vec_cast(x, new_date())
  out <- warp::warp_distance(x, by = "month")

  # warp_distance() returns a double, but for `by = "month"` it
  # is always fits in an integer
  as.integer(out)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.yearmonth character
#' @export
vec_cast.yearmonth.character <- function(x, to, ...) {
  na <- is.na(x)
  x[!na] <- paste0(x[!na], "-01")

  x <- vec_cast(x, new_date())

  vec_cast(x, yearmonth())
}

#' @method vec_cast.character yearmonth
#' @export
vec_cast.character.yearmonth <- function(x, to, ...) {
  format(x)
}
