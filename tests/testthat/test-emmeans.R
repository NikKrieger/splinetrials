
test_that("construct_full_data_set() works", {
  expect_equal(
    construct_full_data_set(fit = test_fit_subgroup),
    dplyr::mutate(
      test_data_subgroup,
      # sch_time_env = time_sch_cont,
      arm = factor(arm, c("control", "active1", "active2")),
      subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3")),
      weights = 1
    )
  )

  # Item shares a name with the weights variable
  names(test_fit_subgroup[["data"]])[ncol(test_fit_subgroup[["data"]])] <- "arm"
  expect_error(
    construct_full_data_set(fit = test_fit_subgroup),
    "cannot have the same name as the weights variable"
  )
})






test_that("ncs_emmeans() works", {

  # scheduled_time and scheduled_time_spec both left blank
  expect_error(
    ncs_emmeans(
      fit = test_fit_subgroup,
      observed_time = "time_obs_cont",
      # scheduled_time = "time_sch_cont",
      arm = "arm",
      subgroup = "subgroup",
      # scheduled_time_spec = unique_times,
      arm_spec = c("control", "active1", "active2"),
      subgroup_spec = c("subgroup1", "subgroup2", "subgroup3"),
      emmeans_args =
        list(
          nesting = NULL,
          params = c("foo", '"subgroup1"', '"subgroup2"', '"subgroup3"',
                     '"control"', '"active1"', '"active2"'),
             nuisance = "continuous1"
        )
    ),
    "cannot be left as the default and requires a value"
  )

  actual_no_subgroup <-
    ncs_emmeans(
      fit = test_fit_no_subgroup,
      observed_time = "time_obs_cont",
      scheduled_time = "time_sch_cont",
      arm = "arm",
      # subgroup = "subgroup",
      # scheduled_time_spec = unique_times,
      arm_spec = c("control", "active1", "active2"),
      # subgroup_spec = c("subgroup1", "subgroup2", "subgroup3"),
      emmeans_args =
        list(
          nesting = NULL,
          params = c(# "foo", '"subgroup1"', '"subgroup2"', '"subgroup3"',
                     '"control"', '"active1"', '"active2"'),
          nuisance = "continuous1")
    )

  actual_with_subgroup <-
    ncs_emmeans(
      fit = test_fit_subgroup,
      observed_time = "time_obs_cont",
      # scheduled_time = "time_sch_cont",
      arm = "arm",
      subgroup = "subgroup",
      scheduled_time_spec = unique_times_subgroup,
      arm_spec = c("control", "active1", "active2"),
      subgroup_spec = c("subgroup1", "subgroup2", "subgroup3"),
      emmeans_args =
        list(nesting = NULL,
             params = c( # "foo",
                        '"subgroup1"', '"subgroup2"', '"subgroup3"',
                        '"control"', '"active1"', '"active2"'))
    )

  expect_equal(actual_no_subgroup, emmeans_results_no_subgroup,
               tolerance = 1e-3)
  expect_equal(actual_with_subgroup, emmeans_results_subgroup,
               tolerance = 1e-3)
})
