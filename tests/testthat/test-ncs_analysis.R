test_that("ncs_analysis() works", {
  expect_snapshot(
    print(test_ncs_analysis, width = Inf, n = Inf)
  )

  expect_equal(
    mmrm::component(
      attr(test_ncs_analysis, "splinetrials_analysis_model"),
      c("neg_log_lik", "beta_est_complete")
    ),
    mmrm::component(
      test_fit_no_subgroup,
      c("neg_log_lik", "beta_est_complete")
    ),
    tolerance = 1e-5
  )
})





test_that("ncs_analysis_subgroup() works", {
  # "between" table
  expect_snapshot(
    print(test_ncs_analysis_subgroup$between, width = Inf, n = Inf)
  )

  # "within" table
  expect_snapshot(
    print(test_ncs_analysis_subgroup$within, width = Inf, n = Inf)
  )

  # Type-III ANOVA table
  expect_snapshot(
    print(test_ncs_analysis_subgroup$type3, width = Inf, n = Inf)
  )

  # Subgroup interaction test table (an anova-class object)
  expect_snapshot(
    print(test_ncs_analysis_subgroup$interaction)
  )

  # Analysis model
  expect_equal(
    mmrm::component(
      test_ncs_analysis_subgroup$analysis_model,
      c("neg_log_lik", "beta_est_complete")
    ),
    mmrm::component(
      test_fit_subgroup,
      c("neg_log_lik", "beta_est_complete")
    )
  )
  # "Full" model
  expect_equal(
    mmrm::component(
      test_ncs_analysis_subgroup$full_model,
      c("neg_log_lik", "beta_est_complete")
    ),
    mmrm::component(
      test_fit_subgroup_full,
      c("neg_log_lik", "beta_est_complete")
    )
  )
  # "Reduced" model
  expect_equal(
    mmrm::component(
      test_ncs_analysis_subgroup$reduced_model,
      c("neg_log_lik", "beta_est_complete")
    ),
    mmrm::component(
      test_fit_subgroup_reduced,
      c("neg_log_lik", "beta_est_complete")
    )
  )
})




test_that("append_correlation_optimizer_cols() adds the correct columns", {
  expect_equal(
    append_correlation_optimizer_cols(
      dplyr::tibble(month.abb, month.name),
      fit = test_fit_subgroup
    ),
    dplyr::tibble(
      month.abb,
      month.name,
      correlation = "heterogeneous unstructured",
      optimizer = "mmrm+tmb"
    )
  )
})



test_that("splinetrials_response_stats_tbl() works", {
  expect_equal(
    splinetrials_response_stats_tbl(
      data = test_data_subgroup,
      env = test_env_subgroup,
      response = quote(response),
      arm = quote(factor(arm, c("control", "active1", "active2"))),
      subgroup = quote(factor(subgroup, c("subgroup1", "subgroup2", "subgroup3"))),
      time_scheduled_label = quote(time_scheduled_label),
      time_scheduled_continuous = quote(time_sch_cont),
      conf.level = 0.95
    ),
    evalq(
      dplyr::arrange(
        dplyr::summarise(
          dplyr::group_by(
            dplyr::as_tibble(test_data_subgroup),
            arm = factor(arm, c("control", "active1", "active2")),
            time = time_scheduled_label,
            time_num = time_sch_cont,
            subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3"))
          ),
          n = dplyr::n(),
          est = mean(response),
          sd = stats::sd(response),
          se = sd / sqrt(n),
          lower = est - !!qnorm(0.975) * se,
          upper = est + !!qnorm(0.975) * se,
          .groups = "drop"
        ),
        as.character(arm),
        time_num
      ),
      test_env_subgroup
    )
  )
})



test_that("join_subgroup_tbls() works", {
  expect_equal(
    join_subgroup_tbls(
      dplyr::tibble(arm = 1:3, time_num = 4:6, subgroup = 7:9, foo = letters[1:3]),
      dplyr::tibble(arm = 3:4, time_num = 6:7, subgroup = 9:10, bar = c("d", "e"))
    ),
    dplyr::tibble(arm = 1:3, time_num = 4:6, subgroup = 7:9, foo = letters[1:3], bar = c(NA, NA, "d"))
  )
})




test_that("prep_between_within_tbl() works", {

  test_table_base <-
    dplyr::tibble(
      arm = factor(c("b", "b", "a", "b", "b", "a")),
      time_num = c(2, 1, 2, 1, 2, 1),
      subgroup = factor(c("Y", "Z", "X", "Y", "Z", "X"))
    )

  test_right_table <-
    dplyr::tibble(
      arm = factor(c("b", "a")),
      time_num = c(1, 2),
      subgroup = factor(c("Z", "Z"))
    )

  expect_equal(
    prep_between_within_tbl(
      test_table_base,
      test_right_table,
      type = "between"
    ),
    dplyr::mutate(
      dplyr::arrange(
        dplyr::left_join(
          test_table_base,
          test_right_table,
          by = c("arm", "time_num", "subgroup")
        ),
        time_num,
        as.character(arm),
        as.character(subgroup)
      ),
      time_num = NULL
    )
  )
})




test_that("prepare_type3_anova_tbl() works", {
  expect_equal(
    prepare_type3_anova_tbl(test_fit_subgroup),
    dplyr::select(
      dplyr::filter(
        dplyr::as_tibble(
          as.data.frame(
            car::Anova(test_fit_subgroup, type = "III", test.statistic = "Chisq")
          ),
          rownames = "effect"
        ),
        effect != "(Intercept)"
      ),
      effect,
      chisquare_test_statistic = Chisq,
      df = Df,
      p_value = `Pr(>Chisq)`
    )
  )
})

test_that("prepare_anova() works", {
  expect_equal(
    prepare_anova(test_fit_subgroup_reduced, test_fit_subgroup_full),
    test_interaction_expected <-
      dplyr::mutate(
        stats::anova(test_fit_subgroup_reduced, test_fit_subgroup_full, test = TRUE, refit = FALSE),
        model = c("reduced model", "full model"),
        aic = AIC,
        bic = BIC,
        loglik = logLik,
        `-2*log(l)` = -2 * logLik,
        test_statistic = 2 * log_likelihood_ratio,
        df = c(NA, diff(df)),
        p_value,
        .keep = "none",
        .before = "df"
      )
  )
})



test_that("splinetrials_analysis_base() works", {

  # With subgroup
  test_splinetrials_analysis_base_actual <-
    splinetrials_analysis_base(
      data = test_data_subgroup,
      response = response,
      subject = patient,
      arm = arm,
      control_group = "control",
      subgroup = subgroup,
      subgroup_comparator = "subgroup1",
      time_observed_continuous = time_obs_cont,
      df = 3,
      spline_basis = NULL,
      time_observed_index = time_observed_index,
      time_scheduled_continuous = time_sch_cont,
      time_scheduled_baseline = 0,
      time_scheduled_label = time_scheduled_label,
      covariates = ~ categorical2 + continuous1,
      cov_structs = "us",
      cov_struct_group = NULL,
      mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb"),
      emmeans_args = list(nesting = NULL),
      average_nuisance = TRUE,
      conf.level = 0.95,
      change_in_bl_contrast_args = list(adjust = "none"),
      treatment_effect_contrast_args = list(adjust = "none"),
      confint_args = list(level = 0.95),
      expand_spline_terms = TRUE,
      caller_env = environment()
    )
  expect_equal(
    names(test_splinetrials_analysis_base_actual),
    c("response_stats_tbl", "emmeans_tbl", "change_from_bl_tbl",
      "between_te_tbl", "within_te_tbl", "percent_slowing_tbl", "fit",
      "formula_parts")
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$response_stats_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$emmeans_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$change_from_bl_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$between_te_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$within_te_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_actual$percent_slowing_tbl, width = Inf, n = Inf)
  )
  expect_equal(
    mmrm::component(test_splinetrials_analysis_base_actual$fit, c("beta_est_complete", "neg_log_lik")),
    mmrm::component(test_fit_subgroup, c("beta_est_complete", "neg_log_lik"))
  )
  expect_equal(
    test_splinetrials_analysis_base_actual$formula_parts,
    list(
      formula_base =
        rlang::set_env(
          response ~ categorical2 + continuous1,
          environment(test_splinetrials_analysis_base_actual$formula_parts$formula_base)
        ),
      spline_call =
        quote((spline_fn(time_obs_cont)[, 1] + spline_fn(time_obs_cont)[, 2] +
                 spline_fn(time_obs_cont)[, 3])),
      arm = quote(arm), # rlang::expr(factor(arm, !!c("control", "active1", "active2"))),
      subgroup = quote(subgroup) # rlang::expr(factor(subgroup, !!c("subgroup1", "subgroup2", "subgroup3")))
    ),
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )




  # Without subgroup
  test_splinetrials_analysis_base_no_subgroup_actual <-
    splinetrials_analysis_base(
      data = test_data_no_subgroup,
      response = response,
      subject = patient,
      arm = arm,
      control_group = "control",
      subgroup = NULL,
      subgroup_comparator = NULL,
      time_observed_continuous = time_obs_cont,
      df = 3,
      spline_basis = NULL,
      time_observed_index = time_observed_index,
      time_scheduled_continuous = time_sch_cont,
      time_scheduled_baseline = 0,
      time_scheduled_label = time_scheduled_label,
      covariates = ~ categorical2 + continuous1,
      cov_structs = "us",
      cov_struct_group = NULL,
      mmrm_args = list(),
      emmeans_args = list(nesting = NULL),
      average_nuisance = TRUE,
      conf.level = 0.95,
      change_in_bl_contrast_args = list(adjust = "none"),
      treatment_effect_contrast_args = list(adjust = "none"),
      confint_args = list(level = 0.95),
      expand_spline_terms = TRUE,
      caller_env = environment()
    )

  expect_equal(
    names(test_splinetrials_analysis_base_no_subgroup_actual),
    c("response_stats_tbl", "emmeans_tbl", "change_from_bl_tbl",
      "treatment_effect_tbl", "percent_slowing_tbl", "fit")
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_no_subgroup_actual$response_stats_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_no_subgroup_actual$emmeans_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_no_subgroup_actual$treatment_effect_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_no_subgroup_actual$treatment_effect_tbl, width = Inf, n = Inf)
  )
  expect_snapshot(
    print(test_splinetrials_analysis_base_no_subgroup_actual$percent_slowing_tbl, width = Inf, n = Inf)
  )
  expect_equal(
    mmrm::component(test_splinetrials_analysis_base_no_subgroup_actual$fit, c("beta_est_complete", "neg_log_lik")),
    mmrm::component(test_fit_no_subgroup, c("beta_est_complete", "neg_log_lik"))
  )
})
