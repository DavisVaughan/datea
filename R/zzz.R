# nocov start

origin <- NULL

.onLoad <- function(libname, pkgname) {
  origin <<- as.Date("1970-01-01")
}

# nocov end
