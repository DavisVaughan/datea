# ------------------------------------------------------------------------------
# Persistent globals

delayedAssign("timeclass_global_empty_ym", new_ym())
delayedAssign("timeclass_global_empty_date", new_date())

# ------------------------------------------------------------------------------

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

class_collapse <- function(x) {
  paste0(class(x), collapse = "/")
}
