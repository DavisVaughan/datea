months_to_days <- function(x) {
  .Call(timeclass_months_to_days, x)
}

months_to_year_month <- function(x) {
  .Call(timeclass_months_to_year_month, x)
}

months_to_year <- function(x) {
  .Call(timeclass_months_to_year, x)
}

months_to_month <- function(x) {
  .Call(timeclass_months_to_month, x)
}
