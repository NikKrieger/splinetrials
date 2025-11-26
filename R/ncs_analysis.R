

#' @rdname ncs_analysis_subgroup
#'
#' @export
ncs_analysis <- function(data,
                          response = "response",
                          subject = "subject",
                          arm = "arm",
                          control_group,
                          time_observed_continuous = "time_observed_continuous",
                          df = 2,
                          spline_basis = NULL,
                          time_observed_index = "time_observed_index",
                          time_scheduled_continuous =
                            "time_scheduled_continuous",
                          time_scheduled_baseline = 0,
                          time_scheduled_label = "time_scheduled_label",
                          covariates = ~ 1,
                          cov_structs = c("us", "toeph", "ar1h", "csh", "cs"),
                          cov_struct_group = NULL,
                          mmrm_args = list(method = "Satterthwaite"),
                          emmeans_args = list(nesting = NULL),
                          average_nuisance = TRUE,
                          conf.level = 0.95,
                          change_in_bl_contrast_args = list(adjust = "none"),
                          treatment_effect_contrast_args =
                            list(adjust = "none"),
                          confint_args = list(level = conf.level),
                          return_models = FALSE,
                          expand_spline_terms = TRUE) {
  caller_env <- rlang::caller_env()
  results <-
    splinetrials_analysis_base(
      data = {{data}},
      response = {{response}},
      subject = {{subject}},
      arm = {{arm}},
      control_group = control_group,
      time_observed_continuous = {{time_observed_continuous}},
      df = df,
      spline_basis = spline_basis,
      time_observed_index = {{time_observed_index}},
      time_scheduled_continuous = {{time_scheduled_continuous}},
      time_scheduled_baseline = time_scheduled_baseline,
      time_scheduled_label = {{time_scheduled_label}},
      covariates = covariates,
      cov_structs = cov_structs,
      cov_struct_group = {{cov_struct_group}},
      mmrm_args = mmrm_args,
      emmeans_args = emmeans_args,
      average_nuisance = average_nuisance,
      conf.level = conf.level,
      change_in_bl_contrast_args = change_in_bl_contrast_args,
      treatment_effect_contrast_args = treatment_effect_contrast_args,
      confint_args = confint_args,
      expand_spline_terms = expand_spline_terms,
      caller_env = caller_env
    )

  out <-
    Reduce(
      function(x, y) dplyr::left_join(x, y, by = c("arm", "time_num")),
      results[c("response_stats_tbl", "emmeans_tbl", "change_from_bl_tbl",
                "treatment_effect_tbl", "percent_slowing_tbl")]
    )

  # Sort alphabetically by the levels of arm, then by the continuous time
  out <- dplyr::arrange(out, as.character(.data[["arm"]]), .data[["time_num"]])

  out[["time_num"]] <- NULL

  out <- append_correlation_optimizer_cols(out, fit = results[["fit"]])

  if (return_models) {
    attr(out, "splinetrials_analysis_model") <- results[["fit"]]
  }

  class(out) <- c("splinetrials_analysis", class(out))

  out
}




# Performs all the main analyses: response variable summary statistics, marginal
# means, change from baseline, treatment effects, and percent slowing.
#
# Results are returned as a list of data frames
splinetrials_analysis_base <- function(data,
                               response,
                               subject,
                               arm,
                               control_group,
                               subgroup = NULL,
                               subgroup_comparator = NULL,
                               time_observed_continuous,
                               df,
                               spline_basis,
                               time_observed_index,
                               time_scheduled_continuous,
                               time_scheduled_baseline,
                               time_scheduled_label,
                               covariates,
                               cov_structs,
                               cov_struct_group,
                               mmrm_args,
                               emmeans_args,
                               average_nuisance,
                               conf.level,
                               change_in_bl_contrast_args,
                               treatment_effect_contrast_args,
                               confint_args,
                               expand_spline_terms,
                               caller_env,
                               analysis_fn_env = rlang::caller_env()) {
  data_expr <- rlang::quo_squash(rlang::enquo(data))
  if (!is.null(control_group)) validate_single_string(control_group)
  response <- validate_user_var({{response}}, call = analysis_fn_env)
  arm <-
    validate_user_var(
      {{arm}},
      require_symbol = TRUE,
      data = data,
      call = analysis_fn_env
    )
  subgroup <-
    validate_user_var(
      {{subgroup}},
      null_ok = TRUE,
      require_symbol = TRUE,
      data = data,
      call = analysis_fn_env
    )
  time_observed_continuous <-
    validate_user_var(
      {{time_observed_continuous}},
      require_symbol = TRUE,
      call = analysis_fn_env
    )
  time_scheduled_continuous <-
    validate_user_var({{time_scheduled_continuous}}, call = analysis_fn_env)
  time_scheduled_label <- validate_user_var({{time_scheduled_label}})

  data_expr <-
    prepare_data_expr(
      data_expr = data_expr,
      col_expr = arm,
      ref_value = control_group,
      data = data,
      env = caller_env
    )

  # subgroup_var is the name of the subgroup column as a character string.
  # subgroup_or_null is "subgroup" when there is a subgroup or NULL if not.
  if (is.null(subgroup)) {
    subgroup_var <- subgroup_or_null <- NULL
  } else {
    subgroup_var <- as.character(subgroup)
    subgroup_or_null <- "subgroup"
    if (!is.null(subgroup_comparator))
      validate_single_string(subgroup_comparator)
    data_expr <-
      prepare_data_expr(
        data_expr = data_expr,
        col_expr = subgroup,
        ref_value = subgroup_comparator,
        data = data,
        env = caller_env
      )
  }

  cov_structs <-
    validate_cov_structs(
      cov_structs = cov_structs,
      time_observed_index = {{time_observed_index}},
      subject = {{subject}},
      cov_struct_group = {{cov_struct_group}},
      data = data,
      call = analysis_fn_env
    )

  if (is.null(spline_basis)) {
    spline_basis <-
      time_spline_basis(
        time = rlang::eval_tidy(time_observed_continuous, data, caller_env),
        df = df
      )
  }

  formula_parts <-
    prepare_formula_parts(
      response = response,
      spline_basis = spline_basis,
      time_observed_continuous = time_observed_continuous,
      arm = arm,
      subgroup = subgroup,
      covariates = covariates,
      caller_env = caller_env,
      expand_spline_terms = expand_spline_terms,
      call = analysis_fn_env
    )

  # Get the character string version of the time, arm, and subgroup columns
  time_observed_continuous <- as.character(time_observed_continuous)
  arm_var <- as.character(arm)

  # Obtain the final analysis model fit
  fit <-
    ncs_mmrm_fit_impl(
      formula_parts = formula_parts,
      type = if (is.null(subgroup)) "basic" else "subgroup_full",
      data_expr = data_expr,
      cov_structs = cov_structs,
      mmrm_args = mmrm_args
    )

  response_stats_tbl <-
    splinetrials_response_stats_tbl(
      data = eval(data_expr, caller_env),
      env = caller_env,
      response = response,
      arm = arm,
      subgroup = subgroup,
      time_scheduled_label = time_scheduled_label,
      time_scheduled_continuous = time_scheduled_continuous,
      conf.level = conf.level,
      call = analysis_fn_env
    )

  scheduled_time_spec <- sort(unique(response_stats_tbl[["time_num"]]))
  arm_spec <- sort(unique(response_stats_tbl[["arm"]]))
  subgroup_spec <- sort(unique(response_stats_tbl[["subgroup"]]))

  emmeans <-
    ncs_emmeans(
      fit = fit,
      observed_time = time_observed_continuous,
      arm = arm_var,
      subgroup = subgroup_var,
      average_nuisance = average_nuisance,
      emmeans_args = emmeans_args,
      scheduled_time_spec = scheduled_time_spec,
      arm_spec = arm_spec,
      subgroup_spec = subgroup_spec
    )
  emmeans_tbl <- as.data.frame(emmeans)
  colnames(emmeans_tbl) <-
    c("arm", "time_num", subgroup_or_null, "response_est",  "response_se",
      "response_df", "response_lower", "response_upper")

  change_from_bl_tbl <-
    change_from_baseline(
      emmeans = emmeans,
      arm = arm_var,
      subgroup = subgroup_var,
      time_observed_continuous = time_observed_continuous,
      time_scheduled_baseline = time_scheduled_baseline,
      contrast_args = change_in_bl_contrast_args,
      as_tibble = TRUE,
      confint_args = confint_args
    )
  colnames(change_from_bl_tbl) <-
    c("arm", "time_num", subgroup_or_null, "change_est", "change_se",
      "change_df", "change_lower", "change_upper", "change_test_statistic",
      "change_p_value")

  treatment_effect_tbl <-
    treatment_effect(
      emmeans = emmeans,
      time_observed_continuous = time_observed_continuous,
      time_scheduled_baseline = time_scheduled_baseline,
      arm = arm_var,
      subgroup = subgroup_var,
      ref_value = control_group,
      subgroup_type = "within",
      contrast_args = treatment_effect_contrast_args,
      as_tibble = TRUE
    )

  percent_slowing_tbl <-
    percent_slowing_using_change_from_bl(
      change_from_bl_tbl = change_from_bl_tbl,
      time_observed_continuous = "time_num",
      arm = "arm",
      control_group = control_group,
      subgroup = subgroup_or_null,
      est = "change_est",
      se = "change_se",
      conf.level = conf.level
    )

  if (is.null(subgroup_var)) {
    colnames(treatment_effect_tbl) <-
      c("arm", "time_num", "diff_est", "diff_se", "diff_df", "diff_lower",
        "diff_upper", "diff_test_statistic", "diff_p_value")

    colnames(percent_slowing_tbl)[c(1L, 2L)] <- c("arm", "time_num")

    out <- list(response_stats_tbl = response_stats_tbl,
                emmeans_tbl = emmeans_tbl,
                change_from_bl_tbl = change_from_bl_tbl,
                treatment_effect_tbl = treatment_effect_tbl,
                percent_slowing_tbl = percent_slowing_tbl,
                fit = fit)
  } else {
    between_te_tbl <-
      treatment_effect(
        emmeans,
        time_scheduled_baseline = time_scheduled_baseline,
        ref_value = subgroup_comparator,
        subgroup_type = "between",
        contrast_args = treatment_effect_contrast_args,
        as_tibble = TRUE,
        confint_args = confint_args
      )
    colnames(between_te_tbl) <-
      c("arm", "time_num", "subgroup", "diff_subgroup_est", "diff_subgroup_se",
        "diff_subgroup_df", "diff_subgroup_lower", "diff_subgroup_upper",
        "diff_subgroup_test_statistic", "diff_subgroup_p_value")

    colnames(treatment_effect_tbl) <-
      c("arm", "time_num", "subgroup", "diff_arm_est", "diff_arm_se",
        "diff_arm_df", "diff_arm_lower", "diff_arm_upper",
        "diff_arm_test_statistic", "diff_arm_p_value")

    colnames(percent_slowing_tbl)[c(1L, 2L, 3L)] <-
      c("arm", "time_num", "subgroup")

    out <- list(response_stats_tbl = response_stats_tbl,
                emmeans_tbl = emmeans_tbl,
                change_from_bl_tbl = change_from_bl_tbl,
                between_te_tbl = between_te_tbl,
                within_te_tbl = treatment_effect_tbl,
                percent_slowing_tbl = percent_slowing_tbl,
                fit = fit,
                formula_parts = formula_parts)
  }

  out
}




# Summary statistics for the response variable, grouped by the unique
# combinations of arm, subgroup, and time_schedueld_continuous.
splinetrials_response_stats_tbl <- function(data,
                                    env,
                                    response,
                                    arm,
                                    subgroup = NULL,
                                    time_scheduled_label,
                                    time_scheduled_continuous,
                                    conf.level,
                                    call = rlang::caller_env()) {
  # These are unevaluated expressions
  var_exprs <-
    list(
      arm = arm,
      time = time_scheduled_label,
      time_num = time_scheduled_continuous,
      response = response
    )
  if (!is.null(subgroup)) var_exprs[["subgroup"]] <- subgroup

  # Evaluate the expressions, first in the context of data and then in the
  # context of env.
  out <- lapply(var_exprs, eval, envir = data, enclos = env)

  out <- dplyr::as_tibble(out)

  validate_scheduled_time(
    data = out,
    time_scheduled_continuous = time_scheduled_continuous,
    time_scheduled_label = time_scheduled_label,
    call = call
  )

  out <- dplyr::group_by(out, dplyr::across(!"response"))
  z <- stats::qnorm(conf.level / 2 + 0.5)

  out <-
    dplyr::summarise(
      out,
      n = dplyr::n(),
      est = mean(.data[["response"]], na.rm = TRUE),
      sd = stats::sd(.data[["response"]], na.rm = TRUE),
      se = .data[["sd"]] / sqrt(.data[["n"]]),
      lower = .data[["est"]] - !!z * .data[["se"]],
      upper = .data[["est"]] + !!z * .data[["se"]],
      .groups = "drop"
    )

  out <- dplyr::arrange(out, as.character(.data[["arm"]]), .data[["time_num"]])

  out
}



# Appends a "correlation" and "optimizer" element to
append_correlation_optimizer_cols <- function(d,
                                              fit,
                                              correlation =
                                                get_cov_type_long(fit),
                                              optimizer = "mmrm+tmb") {
  correlation_name <- make_unique_name("correlation", vec = colnames(d))
  optimizer_name <-
    make_unique_name("optimizer", vec = c(colnames(d), correlation_name))

  d[[correlation_name]] <- correlation
  d[[optimizer_name]] <- optimizer
  d
}



#' `splinetrials_analysis` object
#'
#' [ncs_analysis()] returns an object of class `splinetrials_analysis`: a 32-column
#' [`tibble`][dplyr::tibble] with one row per unique combination of
#' `data[[arm]]` and `data[[time_scheduled_label]]` (see the arguments of
#' [ncs_analysis()]).
#'
#' @details
#'
#' # Columns
#'
#' 1. `arm`: values of `data[[arm]]`.
#'
#' 1. `time`: values of `data[[time_scheduled_label]]`.
#'
#' 1. `n`: number of times the combination appears in data.
#'
#' 1. `est`: [mean] of `data[[response]]`.
#'
#' 1. `sd`: [standard deviation][sd] of `data[[response]]`.
#'
#' 1. `se`: standard error of `data[[response]]` (i.e., `sd / sqrt(n)`).
#'
#' 1. `lower`: lower bound of confidence interval.
#'
#' 1. `upper`: upper bound of confidence interval.
#'
#' 1. `response_est`: estimated marginal mean.
#'
#' 1. `response_se`: standard error of `response_est`.
#'
#' 1. `response_df`: degrees of freedom used for calculating the confidence
#' interval for `response_est`.
#'
#' 1. `response_lower`: lower bound of confidence interval for `response_est`.
#'
#' 1. `response_upper`: upper bound of confidence interval for `response_est`.
#'
#' 1. `change_est`: estimated change from baseline.
#'
#' 1. `change_se`: standard error of `change_est`.
#'
#' 1. `change_df`: degrees of freedom used for calculating the confidence
#' interval for and testing the significance of `change_est`.
#'
#' 1. `change_lower`: lower bound of confidence interval for `change_est`.
#'
#' 1. `change_upper`: upper bound of confidence interval for `change_est`.
#'
#' 1. `change_test_statistic`: test statistic measuring the significance of
#' `change_est`.
#'
#' 1. `change_p_value`: p-value for the significance of `change_est`.
#'
#' 1. `diff_est`: treatment effect.
#'
#' 1. `diff_se`: standard error of `diff_est`.
#'
#' 1. `diff_df`: degrees of freedom used for calculating the confidence interval
#' for and testing the significance of `diff_est`.
#'
#' 1. `diff_lower`: lower bound of confidence interval for `diff_est`.
#'
#' 1. `diff_upper`: upper bound of confidence interval for `diff_est`.
#'
#' 1. `diff_test_statistic`: test statistic measuring the significance of
#' `diff_est`.
#'
#' 1. `diff_p_value`: p-value for the significance of `diff_est`.
#'
#' 1. `percent_slowing_est`: estimated percent slowing.
#'
#' 1. `percent_slowing_lower`: lower bound of confidence interval for
#' `percent_slowing_est`.
#'
#' 1. `percent_slowing_upper`: upper bound of confidence interval for
#' `percent_slowing_est`.
#'
#' 1. `correlation`: the covariance structure of the analysis model. This is the
#' same value repeated for each row.
#'
#' 1. `optimizer`: invariably `mmrm+tmb` to indicate that [mmrm::mmrm()] (which
#' uses the `TMB` package) was used to fit the model.
#'
#' # Optional `analysis_model` attribute
#'
#' If [ncs_analysis()] had `return_models = TRUE`, then the analysis model, an
#' `mmrm` object, will be included as the `analysis_model` [attr]ibute.
#'
#' @seealso The function [ncs_analysis()], which produces objects of this
#'   class.
#'
#' @docType class
#' @name splinetrials_analysis-class
NULL
