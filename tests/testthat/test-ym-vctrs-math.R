
test_that("is.nan() always returns false", {
  expect_identical(is.nan(new_ym(c(1L, NA))), c(FALSE, FALSE))
})

test_that("is.finite() is FALSE for `NA`", {
  expect_identical(is.finite(new_ym(c(1L, NA))), c(TRUE, FALSE))
})

test_that("is.infinite() always returns false", {
  expect_identical(is.infinite(new_ym(c(1L, NA))), c(FALSE, FALSE))
})

# ------------------------------------------------------------------------------

test_that("Summary methods are not available", {
  x <- new_ym(1L)

  expect_error(prod(x))
  expect_error(sum(x))
  expect_error(any(x))
  expect_error(all(x))
})

test_that("Math methods are not available", {
  x <- new_ym(1L)

  expect_error(abs(x))
  expect_error(sign(x))
  expect_error(sqrt(x))
  expect_error(ceiling(x))
  expect_error(floor(x))
  expect_error(trunc(x))
  expect_error(cummax(x))
  expect_error(cummin(x))
  expect_error(cumprod(x))
  expect_error(cumsum(x))
  expect_error(log(x))
  expect_error(log10(x))
  expect_error(log2(x))
  expect_error(log1p(x))
  expect_error(acos(x))
  expect_error(acosh(x))
  expect_error(asin(x))
  expect_error(asinh(x))
  expect_error(atan(x))
  expect_error(atanh(x))
  expect_error(exp(x))
  expect_error(expm1(x))
  expect_error(cos(x))
  expect_error(cosh(x))
  expect_error(cospi(x))
  expect_error(sin(x))
  expect_error(sinh(x))
  expect_error(sinpi(x))
  expect_error(tan(x))
  expect_error(tanh(x))
  expect_error(tanpi(x))
  expect_error(gamma(x))
  expect_error(lgamma(x))
  expect_error(digamma(x))
  expect_error(trigamma(x))
})

test_that("mean() generic is not supported", {
  x <- new_ym(1L)

  expect_error(mean(x))
})

# ------------------------------------------------------------------------------

test_that("math method errors give nice error messages", {
  verify_output(test_path("errors", "test-ym-vctrs-math.txt"), {
    x <- new_ym(1L)

    "# Summary methods are not available"
    prod(x)
    sum(x)
    any(x)
    all(x)

    "# Math methods are not available"
    abs(x)
    sign(x)
    sqrt(x)
    ceiling(x)
    floor(x)
    trunc(x)
    cummax(x)
    cummin(x)
    cumprod(x)
    cumsum(x)
    log(x)
    log10(x)
    log2(x)
    log1p(x)
    acos(x)
    acosh(x)
    asin(x)
    asinh(x)
    atan(x)
    atanh(x)
    exp(x)
    expm1(x)
    cos(x)
    cosh(x)
    cospi(x)
    sin(x)
    sinh(x)
    sinpi(x)
    tan(x)
    tanh(x)
    tanpi(x)
    gamma(x)
    lgamma(x)
    digamma(x)
    trigamma(x)

    "# mean() generic is not supported"
    mean(x)
  })
})
