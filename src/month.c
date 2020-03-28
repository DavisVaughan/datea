#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

static inline int months_to_days_impl(int months);

/*
 * Compute the number of days from `1970-01-01` given a number of months offset
 * from that origin. The number of days correspond to the first of the month
 * that `x` offsets to.
 */
static SEXP months_to_days(SEXP x) {
  R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
  double* p_out = REAL(out);

  int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }

    p_out[i] = months_to_days_impl(elt);
  }

  UNPROTECT(1);
  return out;
}

// [[ export() ]]
SEXP timeclass_months_to_days(SEXP x) {
  return months_to_days(x);
}


static inline void months_to_year_month_impl(int months, int* year, int* month);

static SEXP months_to_year_month(SEXP x) {
  R_xlen_t size = Rf_xlength(x);

  SEXP year = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_year = INTEGER(year);

  SEXP month = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_month = INTEGER(month);

  const int* p_x = INTEGER(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      p_year[i] = NA_INTEGER;
      p_month[i] = NA_INTEGER;
      continue;
    }

    int elt_year;
    int elt_month;

    months_to_year_month_impl(elt, &elt_year, &elt_month);

    p_year[i] = elt_year;
    p_month[i] = elt_month;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, year);
  SET_VECTOR_ELT(out, 1, month);

  UNPROTECT(3);
  return out;
}

// [[ export() ]]
SEXP timeclass_months_to_year_month(SEXP x) {
  return months_to_year_month(x);
}

// -----------------------------------------------------------------------------

static void divmod(int x, int y, int* p_quot, int* p_rem);

#define MONTHS_FROM_0001_01_01_TO_EPOCH 23628
#define DAYS_FROM_0001_01_01_TO_EPOCH 719162
#define MONTHS_IN_YEAR 12

static inline int days_before_year(int year);
static inline int days_before_month(int year, int month);

static inline int months_to_days_impl(int months) {
  months += MONTHS_FROM_0001_01_01_TO_EPOCH;

  int year;
  int month;

  // `month` and `year` come out 0-based but the algorithm wants
  // them 1-based
  divmod(months, MONTHS_IN_YEAR, &year, &month);

  ++year;
  ++month;

  int days = days_before_year(year) + days_before_month(year, month);
  return days - DAYS_FROM_0001_01_01_TO_EPOCH;
}

static inline void months_to_year_month_impl(int months, int* year, int* month) {
  months += MONTHS_FROM_0001_01_01_TO_EPOCH;

  int year_;
  int month_;

  divmod(months, MONTHS_IN_YEAR, &year_, &month_);

  ++year_;
  ++month_;

  *year = year_;
  *month = month_;
}

#undef MONTHS_FROM_0001_01_01_TO_EPOCH
#undef DAYS_FROM_0001_01_01_TO_EPOCH
#undef MONTHS_IN_YEAR

// -----------------------------------------------------------------------------

static int int_div(int x, int y);

static inline int days_before_year(int year) {
  year = year - 1;

  int days = year * 365 +
    int_div(year, 4) -
    int_div(year, 100) +
    int_div(year, 400);

  return days;
}

// -----------------------------------------------------------------------------

#define is_leap_year(year) ((((year) % 4) == 0 && ((year) % 100) != 0) || ((year) % 400) == 0)

static const int DAYS_BEFORE_MONTH[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

static inline int days_before_month(int year, int month) {
  return DAYS_BEFORE_MONTH[month - 1] + (month > 2 && is_leap_year(year));
}

#undef is_leap_year

// -----------------------------------------------------------------------------

// From warp
// https://github.com/DavisVaughan/warp/blob/d7eb3df227e00af68d0dda3a486df0f5ddf15a80/src/date.c#L489
static void divmod(int x, int y, int* p_quot, int* p_rem) {
  if (y == 0) {
    Rf_errorcall(R_NilValue, "Division by zero is not allowed.");
  }

  int quot = x / y;
  int rem = (int)(x - (unsigned int)quot * y);

  if (rem && ((y ^ rem) < 0)) {
    rem += y;
    --quot;
  }

  *p_quot = quot;
  *p_rem = rem;
}

static int int_div(int x, int y) {
  int quot;
  int rem;

  divmod(x, y, &quot, &rem);

  return quot;
}
