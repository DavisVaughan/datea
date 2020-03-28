months_to_days <- function(x) {
  .Call(timeclass_months_to_days, x)
}

months_to_year_month <- function(x) {
  .Call(timeclass_months_to_year_month, x)
}
