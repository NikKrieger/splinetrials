
test_that("add_ref_index_col() works", {
  expect_equal(
    get_ref_indices(grid = mtcars, var = "mpg", ref_value = 33.9),
    rep_len(20, 32L)
  )

  grid <-
    expand.grid(
      lc = c("c", "a", "b"),
      uc = c("A", "B"),
      tc = c(1, 0, 2),
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    )

  expect_equal(
    get_ref_indices(
      grid = grid,
      var = "lc",
      ref_value = "c",
      grouping = c("uc", "tc")
    ),
    rep(seq(1, by = 3, length.out = 6), each = 3)
  )
})




test_that("ncs_contrasts() works", {

  # Calculate expected contrasts results
  grid <-
    emmeans_results_subgroup@grid[c("arm", "time_obs_cont", "subgroup")]
  method_base <- diag(nrow(grid))
  rownames(method_base) <- colnames(method_base) <-
    with(grid, paste(arm, time_obs_cont, subgroup, sep = "_"))
  for (r in seq_len(nrow(grid))) {
    bl_row <-
      with(grid, time_obs_cont == 0 & arm == arm[r] & subgroup == subgroup[r])
    method_base[bl_row, r] <- -1
  }

  # Change from baseline
  method_change_from_bl <- method_base[, grid$time_obs_cont != 0]
  method_change_from_bl <- as.list(as.data.frame(method_change_from_bl))
  change_from_bl_expected <-
    emmeans::contrast(
      object = emmeans_results_subgroup,
      method = method_change_from_bl,
      adjust = "none"
    )

  # Treatment effect: between
  method_trt_between <- method_base
  for (r in seq_len(nrow(grid))) {
    ref_row <-
      with(
        grid,
        subgroup == "subgroup1" & arm == arm[r] &
          time_obs_cont == time_obs_cont[r]
      )
    method_trt_between[ref_row, r] <- -1
    ref_bl_row <-
      with(grid, time_obs_cont == 0 & subgroup == "subgroup1" & arm == arm[r])
    method_trt_between[ref_bl_row, r] <- +1
  }
  method_trt_between <-
    method_trt_between[, grid$subgroup != "subgroup1" & grid$time_obs_cont != 0]
  method_trt_between <- as.list(as.data.frame(method_trt_between))
  trt_between_expected <-
    emmeans::contrast(
      object = emmeans_results_subgroup,
      method = method_trt_between,
      adjust = "none"
    )

  # Treatment effect: within
  method_trt_within <- method_base
  for (r in seq_len(nrow(grid))) {
    ref_row <-
      with(
        grid,
        arm == "control" & subgroup == subgroup[r] &
          time_obs_cont == time_obs_cont[r]
      )
    method_trt_within[ref_row, r] <- -1
    ref_bl_row <-
      with(
        grid,
        time_obs_cont == 0 & arm == "control" & subgroup == subgroup[r]
      )
    method_trt_within[ref_bl_row, r] <- +1
  }
  method_trt_within <-
    method_trt_within[, grid$arm != "control" & grid$time_obs_cont != 0]
  method_trt_within <- as.list(as.data.frame(method_trt_within))
  trt_within_expected <-
    emmeans::contrast(
      object = emmeans_results_subgroup,
      method = method_trt_within,
      adjust = "none"
    )


  # Change from baseline - using ncs_contrasts()
  expect_equal(
    ncs_contrasts(
      emmeans = emmeans_results_subgroup,
      type = "change_from_bl",
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup"
    ),
    change_from_bl_expected,
    tolerance = 1e-3
  )
  # Change from baseline - using change_from_baseline()
  expect_equal(
    change_from_baseline(
      emmeans = emmeans_results_subgroup,
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup"
    ),
    change_from_bl_expected,
    tolerance = 1e-3
  )
  # Change from baseline - as tibble
  change_from_baseline_tibble_actual <-
    ncs_contrasts(
      emmeans = emmeans_results_subgroup,
      type = "change_from_bl",
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup",
      as_tibble = TRUE
    )
  expect_snapshot(
    print(change_from_baseline_tibble_actual, width = Inf, n = Inf)
  )
  # Change from baseline - as tibble - no confidence interval
  expect_snapshot(
    print(
      ncs_contrasts(
        emmeans = emmeans_results_subgroup,
        type = "change_from_bl",
        time_observed_continuous = "time_obs_cont",
        time_scheduled_baseline = 0,
        arm = "arm",
        subgroup = "subgroup",
        as_tibble = TRUE,
        confint_args = NULL
      ),
      width = Inf,
      n = Inf
    )
  )



  # Treatment effect - between - using ncs_contrasts()
  expect_equal(
    ncs_contrasts(
      emmeans = emmeans_results_subgroup,
      type = "treatment_effect",
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup",
      ref_value = "subgroup1",
      subgroup_type = "between"
    ),
    trt_between_expected,
    tolerance = 1e-3
  )
  # Treatment effect - between - using treatment_effect()
  expect_equal(
    treatment_effect(
      emmeans = emmeans_results_subgroup,
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup",
      ref_value = "subgroup1",
      subgroup_type = "between"
    ),
    trt_between_expected,
    tolerance = 1e-3
  )
  # Treatment effect - within - using ncs_contrasts()
  expect_equal(
    ncs_contrasts(
      emmeans = emmeans_results_subgroup,
      type = "treatment_effect",
      time_observed_continuous = "time_obs_cont",
      time_scheduled_baseline = 0,
      arm = "arm",
      subgroup = "subgroup",
      ref_value = "control",
      subgroup_type = "within"
    ),
    trt_within_expected,
    tolerance = 1e-3
  )


  # Percent slowing
  cfbl_trt_wide <-
    dplyr::as_tibble(
      merge(
        dplyr::filter(change_from_baseline_tibble_actual, arm != "control"),
        dplyr::filter(change_from_baseline_tibble_actual, arm == "control"),
        by = c("time_obs_cont", "subgroup"),
        all.x = TRUE,
        suffixes = c("_trt", "_ctl"),
        sort = FALSE
      )
    )
  theta <- cfbl_trt_wide[["estimate_trt"]] / cfbl_trt_wide[["estimate_ctl"]]
  moe <-
    with(
      cfbl_trt_wide,
      100 * stats::qnorm(0.975) *
        sqrt(SE_trt ^ 2 + (theta * SE_ctl) ^ 2) / abs(estimate_ctl)
    )
  percent_slowing_expected <-
    dplyr::reframe(
      cfbl_trt_wide,
      arm = arm_trt,
      time_obs_cont,
      subgroup,
      percent_slowing_est = 100 - 100 * estimate_trt / estimate_ctl,
      percent_slowing_lower = percent_slowing_est - moe,
      percent_slowing_upper = percent_slowing_est + moe
    )
  expect_equal(
    percent_slowing_using_change_from_bl(
      change_from_bl_tbl = change_from_baseline_tibble_actual,
      time_observed_continuous = "time_obs_cont",
      arm = "arm",
      control_group = "control",
      subgroup = "subgroup"
    ),
    percent_slowing_expected
  )
})
