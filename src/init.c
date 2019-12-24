// File generated automatically by cbuild - please do not modify by hand

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

// .Call declarations
extern SEXP timeclass_months_to_days(SEXP);

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  {"timeclass_months_to_days", (DL_FUNC) &timeclass_months_to_days, 1},
  {NULL, NULL, 0}
};

void R_init_timeclass(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

