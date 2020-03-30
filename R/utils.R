# ------------------------------------------------------------------------------
# Persistent globals

delayedAssign("timeclass_global_empty_ym", new_ym())
delayedAssign("timeclass_global_empty_date", new_date())
delayedAssign("timeclass_global_origin_date", new_date(0))

# ------------------------------------------------------------------------------

tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}

class_collapse <- function(x) {
  paste0(class(x), collapse = "/")
}
