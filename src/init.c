// File generated automatically by cbuild - please do not modify by hand

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

// .Call declarations
extern SEXP datea_months_to_days(SEXP);
extern SEXP datea_months_to_year_month(SEXP);
extern SEXP datea_months_to_year(SEXP);
extern SEXP datea_months_to_month(SEXP);

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  {"datea_months_to_days",       (DL_FUNC) &datea_months_to_days, 1},
  {"datea_months_to_year_month", (DL_FUNC) &datea_months_to_year_month, 1},
  {"datea_months_to_year",       (DL_FUNC) &datea_months_to_year, 1},
  {"datea_months_to_month",      (DL_FUNC) &datea_months_to_month, 1},
  {NULL, NULL, 0}
};

void R_init_datea(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

