
# Registered in .onLoad()
tz.ym <- function(x) {
  "UTC"
}

# Only implemented to have better handling of extra indices passed in `...`
#' @export
`[.ym` <- function(x, i, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  i <- maybe_missing(i, TRUE)

  vec_slice(x, i)
}


# Slightly faster than `vec_proxy.vctrs_vctr` since we know it isn't a list
#' @export
vec_proxy.ym <- function(x, ...) {
  x
}
