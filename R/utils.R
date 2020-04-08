# ------------------------------------------------------------------------------
# Persistent globals

delayedAssign("timeclass_global_empty_ym", new_ym())
delayedAssign("timeclass_global_empty_date", new_date())
delayedAssign("timeclass_global_origin_date", new_date(0))
delayedAssign("timeclass_global_origin_posixct", new_datetime(0, "UTC"))
delayedAssign("timeclass_global_seconds_in_day", 86400)

# ------------------------------------------------------------------------------

tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}

class_collapse <- function(x) {
  paste0(class(x), collapse = "/")
}

is_one_dim <- function(x) {
  # Bypass `dim()` dispatch (for data frames)
  dim <- attr(x, "dim", exact = TRUE)

  if (is.null(dim)) {
    return(TRUE)
  }

  if (length(dim) == 1L) {
    return(TRUE)
  }

  FALSE
}

glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

stop_requires_ym <- function(x) {
  class <- class_collapse(x)
  glubort("`x` must be a <ym>, not a <{class}>.")
}
