#' @export
#' @rdname vctrs-compat
#' @method vec_cast ym
#' @export vec_cast.ym
vec_cast.ym <- function(x, to, ...) {
  UseMethod("vec_cast.ym")
}

#' @method vec_cast.ym default
#' @export
vec_cast.ym.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym ym
#' @export
vec_cast.ym.ym <- function(x, to, ...) {
  x
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym Date
#' @export
vec_cast.ym.Date <- function(x, to, ...) {
  months <- warp_distance(x, by = "month")
  ym(months)
}

#' @method vec_cast.Date ym
#' @export
vec_cast.Date.ym <- function(x, to, ...) {
  new_date(vec_data(x))
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym POSIXct
#' @export
vec_cast.ym.POSIXct <- function(x, to, ...) {
  # Drop time zone before passing to warp_distance()
  x <- as.Date(x)
  months <- warp_distance(x, by = "month")
  ym(months)
}

#' @method vec_cast.POSIXct ym
#' @export
vec_cast.POSIXct.ym <- function(x, to, ...) {
  x <- vec_cast(x, new_date())
  vec_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym POSIXlt
#' @export
vec_cast.ym.POSIXlt <- function(x, to, ...) {
  # Drop time zone before passing to warp_distance()
  x <- as.Date(x)
  months <- warp_distance(x, by = "month")
  ym(months)
}

#' @method vec_cast.POSIXlt ym
#' @export
vec_cast.POSIXlt.ym <- function(x, to, ...) {
  x <- vec_cast(x, new_date())
  vec_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym double
#' @export
vec_cast.ym.double <- function(x, to, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_cast(x, to)
  }

  ym(x)
}

#' @method vec_cast.double ym
#' @export
vec_cast.double.ym <- function(x, to, ...) {
  if (!is_one_dim(to)) {
    stop_incompatible_cast(x, to)
  }

  x <- vec_cast(x, new_date())
  warp::warp_distance(x, by = "month")
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym integer
#' @export
vec_cast.ym.integer <- function(x, to, ...) {
  if (!is_one_dim(x)) {
    stop_incompatible_cast(x, to)
  }

  ym(x)
}

#' @method vec_cast.integer ym
#' @export
vec_cast.integer.ym <- function(x, to, ...) {
  if (!is_one_dim(to)) {
    stop_incompatible_cast(x, to)
  }

  x <- vec_cast(x, new_date())
  out <- warp_distance(x, by = "month")

  # warp_distance() returns a double, but for `by = "month"` it
  # is always fits in an integer
  as.integer(out)
}

# ------------------------------------------------------------------------------

#' @method vec_cast.ym character
#' @export
vec_cast.ym.character <- function(x, to, ...) {
  na <- is.na(x)
  x[!na] <- paste0(x[!na], "-01")

  x <- vec_cast(x, new_date())

  vec_cast(x, ym())
}

#' @method vec_cast.character ym
#' @export
vec_cast.character.ym <- function(x, to, ...) {
  format(x)
}
