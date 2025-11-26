test_that("validate_flag() works", {
  expect_error(validate_flag(letters), "object of class")
  expect_error(validate_flag(NA), "It is .?NA.?\\.")
  expect_error(validate_flag(c(TRUE, FALSE)), "logical vector of length 2")
  expect_true(validate_flag(TRUE))
})

test_that("validate_numeric() works", {
  expect_error(validate_numeric("a"), "object of class")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), min_length = 5), "must have at least")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), max_length = 3), "must have at most")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), exact_length = 2), "must have exactly")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), NA_ok = FALSE), "must not contain missing")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), Inf_ok = FALSE), "must not contain Inf")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), duplicates_ok = FALSE), "must not contain dup")
  expect_error(validate_numeric(c(NA, Inf, -Inf, Inf), unsorted_ok = FALSE), "must be sorted")
  expect_equal(validate_numeric(c(pi, Inf)), c(pi, Inf))
})

test_that("validate_covariates() works", {
  expect_error(validate_covariates(list()), "object of class")
  expect_error(validate_covariates(a ~ b), "It has a lefthand")
  expect_error(validate_covariates(~ a + us(b | c / d)), "covariance structure")
  expect_equal(validate_covariates(~ 1), ~ 1)
})



test_that("validate_cov_structs() works", {
  expect_error(validate_cov_structs(NA), "object of class")
  expect_error(validate_cov_structs(character()), "at least one element")
  expect_error(validate_cov_structs("invalid"), "must be in the set")
  expect_error(
    validate_cov_structs(list(c, t)),
    "all its elements must be .?cov_struct.? objects"
  )
  expect_error(
    validate_cov_structs(rep(list(mmrm::cov_struct("us", "v", "s")), 2)),
    "must not contain duplicates"
  )

  # Without a cov_struct_group
  expect_equal(
    validate_cov_structs(
      c("us", "cs"),
      time_observed_index = paste0(AVISIT, "abc"),
      subject = identity(USUBJID),
      data = mmrm::fev_data
    ),
    lapply(
      c("us", "cs"),
      function(type) {
        substitute(
          mmrm::cov_struct(
            type = foo,
            visits = 'as.ordered(paste0(AVISIT, "abc"))',
            subject = "identity(USUBJID)",
            group = character()
          ),
          list(foo = type)
        )
      }
    )
  )

  # With a cov_struct_group
  expect_equal(
    validate_cov_structs(
      c("us", "cs"),
      time_observed_index = paste0(AVISIT, "abc"),
      subject = identity(USUBJID),
      cov_struct_group = 1 + 1,
      data = mmrm::fev_data
    ),
    lapply(
      c("us", "cs"),
      function(type) {
        substitute(
          mmrm::cov_struct(
            type = foo,
            visits = 'as.ordered(paste0(AVISIT, "abc"))',
            subject = "identity(USUBJID)",
            group = "as.factor(1 + 1)"
          ),
          list(foo = type)
        )
      }
    )
  )
})



test_that("prepare_mmrm_args() works", {
  expect_error(prepare_mmrm_args(list(formula = ~ 1)), "Cannot supply a .?formula")
  expect_equal(
    prepare_mmrm_args(list(a = 1), formula = ~ 1, data = quote(foo)),
    list(a = 1, formula = ~ 1, data = quote(foo))
  )
})




test_that("validate_single_string() works", {
  expect_error(validate_single_string(1), "object of class")
  expect_error(validate_single_string(letters), "character vector of length 26")
  expect_error(validate_single_string("foo", data = mtcars), "not found among")
  expect_equal(validate_single_string("mpg", data = mtcars), "mpg")
})



test_that("validate_user_var() works", {
  expect_null(validate_user_var(NULL, null_ok = TRUE))
  expect_error(validate_user_var(NULL, null_ok = FALSE), "It is .?NULL")
  expect_equal(validate_user_var("a"), quote(a))
  expect_error(validate_user_var(a(b), require_symbol = TRUE), "It is a call")
  expect_error(validate_user_var(1, require_symbol = TRUE), "object of type")
  expect_equal(
    validate_user_var(
      AVISIT,
      make_factor_call = "as.ordered",
      data = mmrm::fev_data
    ),
    quote(as.ordered(AVISIT))
  )
})




test_that("validate_scheduled_time() works", {
  expect_error(
    validate_scheduled_time(
      dplyr::tibble(time = c(1, 1, 2), time_num = letters[1:3]),
      time_scheduled_continuous = quote(tsc),
      time_scheduled_label = quote(tsl)
    ),
    "must be a one-to-one relationship between the .?time_scheduled_continuous"
  )
  expect_error(
    validate_scheduled_time(
      dplyr::tibble(time = 1:3, time_num = c("a", "a", "b")),
      time_scheduled_continuous = quote(tsc),
      time_scheduled_label = quote(tsl)
    ),
    "must be a one-to-one relationship between the .?time_scheduled_label"
  )
  expect_equal(
    validate_scheduled_time(dplyr::tibble(time = 1:3, time_num = letters[1:3])),
    dplyr::tibble(time = 1:3, time_num = letters[1:3])
  )
})


test_that("validate_ref_value() works", {
  expect_error(
    validate_ref_value(
      ref_value = "foo",
      grid = mmrm::fev_data,
      var_name = "ARMCD"
    ),
    'foo.? was not found in .?ARMCD'
  )
  expect_equal(
    validate_ref_value(
      ref_value = "TRT",
      grid = mmrm::fev_data,
      var_name = "ARMCD"
    ),
    "TRT"
  )
})



test_that("validate_arg_list() works", {
  expect_error(validate_arg_list(c), "object of type")
  expect_error(validate_arg_list(list(a = 1, 2)), "empty or missing name")
  expect_error(validate_arg_list(list(a = 1, a = 2)), "Multiple arguments have")
  expect_equal(validate_arg_list(list(a = 1, b = 2)), list(a = 1, b = 2))
})


test_that("validate_character() works", {
  expect_error(validate_character(NA), "object of class")
  expect_error(validate_character(letters, min_length = 27), "must have at least")
  expect_error(validate_character(letters, max_length = 25), "must have at most")
  expect_error(validate_character(letters, exact_length = 25), "must have exactly")
  expect_error(validate_character(NA_character_, NA_ok = FALSE), "must not contain missing")
  expect_error(validate_character("", empty_strings_ok = FALSE), "must not contain empty strings")
  expect_error(validate_character(c("a", "a"), duplicates_ok = FALSE), "must not contain dup")
  expect_error(validate_character(c("b", "a"), unsorted_ok = FALSE), "must be sorted")
  expect_equal(validate_character(letters), letters)
})



test_that("validate_emmeans_spec() works", {
  expect_error(
    validate_emmeans_spec(
      spec = c("a", "b", "c", "d"),
      var = "subgroup",
      data = dplyr::mutate(test_data_subgroup, subgroup = as.factor(subgroup))
    ),
    "must match the factor levels of"
  )
  expect_equal(
    validate_emmeans_spec(
      spec = c("control", "active1", "active2"),
      var = "arm",
      data = dplyr::mutate(test_data_subgroup, arm = factor(arm, c("control", "active1", "active2")))
    ),
    c("control", "active1", "active2")
  )
})
