
test_that("caller_arg() yields the expression supplied to an argument", {
  fn <- function(x, `{{`) caller_arg(x, `{{` = `{{`)
  expect_equal(fn(a, `{{` = FALSE), "a")

  # The !! operator is necessary here only because testthat::expect_equal() uses
  # enquo() on its first argument.
  expect_equal(!!fn({{b}}, `{{` = TRUE), "b")
})

test_that("make_unique_name() creates new name", {
  expect_equal(
    make_unique_name(
      "sd",
      env = as.environment("package:stats"),
      vec = c("sd.1", "foo")
    ),
    "sd.2"
  )
})


test_that("make_factor_call() works", {
  expect_equal(
    make_factor_call(
      quote(gender),
      fn = "as.factor",
      data = dplyr::starwars,
      env = environment()
    ),
    quote(as.factor(gender))
  )
  expect_equal(
    make_factor_call(
      quote(gender),
      fn = "as.ordered",
      data = dplyr::starwars,
      env = environment()
    ),
    quote(as.ordered(gender))
  )
})



test_that("get_cov_type_long() returns correct long covariance name", {
  expect_equal(
    get_cov_type_long(test_fit_subgroup),
    "heterogeneous unstructured"
  )
})
