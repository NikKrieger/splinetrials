
test_that("prepare_data_expr() works", {
  expect_equal(
    data_expr <-
      prepare_data_expr(
        data_expr = quote(test_data_subgroup),
        col_expr = quote(arm),
        ref_value = "control",
        data = test_data_subgroup,
        env = environment()
      ),
    rlang::expr(
      dplyr::mutate(
        test_data_subgroup,
        arm = factor(arm, !!c("control", "active1", "active2"))
      )
    )
  )

  expect_equal(
    data_expr <-
      prepare_data_expr(
        data_expr = data_expr,
        col_expr = quote(subgroup),
        ref_value = "subgroup1",
        data = test_data_subgroup,
        env = environment()
      ),
    rlang::expr(
      dplyr::mutate(
        test_data_subgroup,
        arm = factor(arm, !!c("control", "active1", "active2")),
        subgroup = factor(subgroup, !!c("subgroup1", "subgroup2", "subgroup3"))
      )
    )
  )

  expect_equal(
    data_expr <-
      prepare_data_expr(
        data_expr = data_expr,
        col_expr = quote(arm),
        ref_value = "active2",
        data = test_data_subgroup,
        env = environment()
      ),
    rlang::expr(
      dplyr::mutate(
        test_data_subgroup,
        arm = factor(arm, !!c("control", "active1", "active2")),
        subgroup = factor(subgroup, !!c("subgroup1", "subgroup2", "subgroup3")),
        arm = factor(arm, !!c("active2", "active1", "control"))
      )
    )
  )

  expect_equal(
    prepare_data_expr(
      data_expr = data_expr,
      col_expr = quote(arm),
      ref_value = "active2",
      data = eval(data_expr),
      env = environment()
    ),
    data_expr
  )
})


test_that("create_formula_base() works", {
  expect_equal(
    create_formula_base(
      response = quote(log(y)),
      covariates = ~ a + sqrt(b),
      env = environment()
    ),
    log(y) ~ a + sqrt(b)
  )
})



test_that("create_spline_call() works", {
  # Expand spline terms
  expect_equal(
    create_spline_call(
      spline_fn_name = "s",
      time_observed_continuous = quote(t),
      expand_spline_terms = TRUE,
      spline_basis = test_basis
    ),
    quote((s(t)[, 1] + s(t)[, 2] + s(t)[, 3]))
  )

  # Do not expand spline terms
  expect_equal(
    create_spline_call(
      spline_fn_name = "s",
      time_observed_continuous = quote(t),
      expand_spline_terms = FALSE,
      spline_basis = test_basis
    ),
    quote(s(t))
  )
})



test_that("prepare_formula() works", {
  expect_equal(
    prepare_formula(
      type = "basic",
      formula_base = y ~ x + z,
      spline_call = quote(s(t)),
      arm = quote(a)
    ),
    y ~ s(t) + x + z + s(t):a
  )
  expect_equal(
    prepare_formula(
      type = "subgroup_reduced",
      formula_base = y ~ x + z,
      spline_call = quote(s(t)),
      arm = quote(a),
      subgroup = quote(sg)
    ),
    y ~ s(t) + sg + x + z + s(t):sg + s(t):a
  )
  expect_equal(
    prepare_formula(
      type = "subgroup_full",
      formula_base = y ~ x + z,
      spline_call = quote(s(t)),
      arm = quote(a),
      subgroup = quote(sg)
    ),
    y ~ s(t) + sg + x + z + s(t):sg + s(t):a + s(t):sg:a
  )
})



test_that("fit_cov_struct() works", {
  test_mmrm_call <-
    rlang::expr(
      mmrm::mmrm(
        formula = !!(FEV1 ~ AVISIT),
        data = test_data_convergence_failure,
        weights = rep_len(1, 5),
        optimizer = "L-BFGS-B",
        covariance = mmrm::cov_struct(type = "us", "AVISIT", "USUBJID")
      )
    )

  # Model fitting attempt that errorred out for any reason other than
  # non-convergence
  expect_error(
    fit_cov_struct(
      mmrm_args =
        list(
          formula = FEV1 ~ 1,
          data = quote(test_data_convergence_failure)
        ),
      covariance =
        quote(mmrm::cov_struct(type = "us", "as.ordered(AVISIT)", "USUBJID")),
      env = environment()
    ),
    "must not lead to an"
  )

  # Model fit that had could not convergence
  expect_equal(
    fit_cov_struct(
      mmrm_args =
        list(
          formula = FEV1 ~ AVISIT,
          data = quote(test_data_convergence_failure),
          weights = quote(rep_len(1, 5)),
          optimizer = "L-BFGS-B"
        ),
      covariance = quote(mmrm::cov_struct(type = "us", "AVISIT", "USUBJID")),
      env = environment()
    ),
    structure(
      test_mmrm_call,
      env = environment(),
      converged = FALSE,
      error_message =
        simpleError(
          message =
            "Chosen optimizers 'L-BFGS-B' led to problems during model fit:\n1) Model convergence problem: hessian is singular, theta_vcov not available.\nConsider trying multiple or different optimizers.",
          call = test_mmrm_call
        )
    )
  )


  # Successful model fit
  actual <- fit_cov_struct(
    mmrm_args = alist(
      formula = FEV1 ~ FEV1_BL + SEX * RACE,
      data = mmrm::fev_data,
      method = "Satterthwaite"
    ),
    covariance =
      quote(
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(VISITN)", # "as.ordered(paste0(sch_time_env, foo))",
          subject = "USUBJID",
          group = character()
        )
      ),
    env = environment()
  )
  expected <-
    mmrm::mmrm(
      formula = FEV1 ~ FEV1_BL + SEX * RACE,
      data = mmrm::fev_data,
      method = "Satterthwaite",
      covariance =
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(VISITN)", # "as.ordered(paste0(sch_time_env, foo))",
          subject = "USUBJID",
          group = character()
        )
    )
  expect_equal(
    mmrm::component(actual, c("neg_log_lik", "beta_est_complete")),
    mmrm::component(expected, c("neg_log_lik", "beta_est_complete")),
    tolerance = 1e-3
  )
})




test_that("prepare_formula_parts() works", {
  test_formula_parts_actual <-
    prepare_formula_parts(
      response = quote(response),
      spline_basis = test_basis,
      time_observed_continuous = quote(time_obs_cont),
      arm = quote(arm),
      subgroup = quote(subgroup),
      covariates = ~ categorical2 + continuous1,
      caller_env = environment(),
      expand_spline_terms = TRUE
    )

  # Expanding spline terms
  test_formula_parts_expected <-
    list(
      formula_base =
        rlang::set_env(
          response ~ categorical2 + continuous1,
          environment(test_formula_parts_actual$formula_base)
        ),
      spline_call =
        quote((spline_fn(time_obs_cont)[, 1] + spline_fn(time_obs_cont)[, 2] +
                 spline_fn(time_obs_cont)[, 3])),
      arm = quote(arm),
      subgroup = quote(subgroup)
    )
  expect_equal(
    test_formula_parts_actual,
    test_formula_parts_expected
  )

  # Don't expand spline terms
  actual <-
    prepare_formula_parts(
      response = quote(log(y)),
      spline_basis = test_basis,
      time_observed_continuous = quote(t),
      arm = quote(a),
      subgroup = NULL,
      covariates = ~ x + sqrt(z),
      caller_env = environment(),
      expand_spline_terms = FALSE
    )
  expect_equal(
    actual,
    list(
      formula_base =
        rlang::set_env(log(y) ~ x + sqrt(z), environment(actual$formula_base)),
      spline_call = quote(spline_fn(t)),
      arm = quote(a),
      subgroup = NULL
    )
  )
})



test_that("ncs_mmrm_fit_impl() works", {
  test_formula_parts <-
    list(
      formula_base = FEV1 ~ 1,
      spline_call = quote(FEV1_BL),
      arm = quote(SEX),
      subgroup = NULL
    )

  test_cov_struct_list <-
    alist(
      mmrm::cov_struct(type = "us", "as.ordered(AVISIT)", "USUBJID"),
      mmrm::cov_struct(type = "ar1", "as.ordered(AVISIT)", "USUBJID")
    )

  # No successful convergence
  expect_error(
    suppressMessages(
      ncs_mmrm_fit_impl(
        formula_parts = test_formula_parts,
        type = "subgroup_full",
        data_expr = quote(test_data_convergence_failure),
        cov_structs = test_cov_struct_list[1],
        mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb")
      )
    ),
    "None of the covariance structure types"
  )

  # Message that a covariance structure failed to converge
  expect_message(
    actual <-
      ncs_mmrm_fit_impl(
        formula_parts = test_formula_parts,
        type = "subgroup_full",
        data_expr = quote(test_data_convergence_failure),
        cov_structs = test_cov_struct_list,
        mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb")
      ),
    "did not successfully converge"
  )

  # Successful model fitting
  expected <-
    mmrm::mmrm(
      formula = FEV1 ~ FEV1_BL + FEV1_BL:SEX,
      data = test_data_convergence_failure,
      covariance =
        mmrm::cov_struct(type = "ar1", "as.ordered(AVISIT)", "USUBJID"),
      method = "Satterthwaite",
      optimizer = "nlminb",
      vcov = "Empirical-Bias-Reduced"
    )
  expect_equal(
    mmrm::component(actual, c("neg_log_lik", "beta_est_complete")),
    mmrm::component(expected, c("neg_log_lik", "beta_est_complete"))
  )
})


test_that("converged() works", {
  expect_equal(converged(structure(list(), converged = "foo")), "foo")
})



test_that("ncs_mmrm_fit() works", {

  # Error: subgroup is NULL but type is "subgroup_full"
  expect_error(
    ncs_mmrm_fit(
      data = test_data_subgroup,
      type = "subgroup_full",
      response = "response",
      subject = "patient",
      arm = arm,
      control_group = "control",
      subgroup = NULL,
      subgroup_comparator = "subgroup1",
      time_observed_continuous = "time_obs_cont",
      time_observed_index = time_observed_index,
      df = 3,
      covariates = ~ categorical2 + continuous1,
      cov_structs = "us",
      mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb")
    ),
    "If \\W*?subgroup\\W*? is \\W*?NULL\\W*?"
  )

  # Error: subgroup is not NULL and type is "basic"
  expect_error(
    ncs_mmrm_fit(
      data = test_data_subgroup,
      type = "basic",
      response = "response",
      subject = "patient",
      arm = arm,
      control_group = "control",
      subgroup = subgroup,
      subgroup_comparator = "subgroup1",
      time_observed_continuous = "time_obs_cont",
      time_observed_index = time_observed_index,
      df = 3,
      covariates = ~ categorical2 + continuous1,
      cov_structs = "us",
      mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb")
    ),
    "If \\W*?subgroup\\W*? is not \\W*?NULL\\W*?"
  )

  # Successful fit
  test_actual <-
    ncs_mmrm_fit(
      data = test_data_subgroup,
      type = "subgroup_full",
      response = "response",
      subject = "patient",
      arm = arm,
      control_group = "control",
      subgroup = "subgroup",
      subgroup_comparator = "subgroup1",
      time_observed_continuous = "time_obs_cont",
      time_observed_index = time_observed_index,
      df = 3,
      covariates = ~ categorical2 + continuous1,
      cov_structs = "us",
      mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb")
    )
  expect_equal(
    mmrm::component(test_actual, c("neg_log_lik", "beta_est_complete")),
    mmrm::component(test_fit_subgroup, c("neg_log_lik", "beta_est_complete"))
  )
})
