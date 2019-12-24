tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}

vec_dim <- function(x) {
  dim <- dim(x)

  if (is.null(dim)) {
    return(1L)
  }

  length(dim)
}

is_one_dim <- function(x) {
  vec_dim(x) == 1L
}
