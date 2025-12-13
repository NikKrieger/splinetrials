

#' Run a Natural Cubic Spline (NCS) Analysis.
#'
#' @description Fit and analyze an [`mmrm`][mmrm::mmrm] model wherein the
#'   continuous time variable has splines applied.
#'
#' - `ncs_analysis()` fits such a model without involving subgroups.
#'
#' - `ncs_analysis_subgroup()` fits a model that involves subgroups and
#'   performs additional analyses.
#'
#' @param data (`data frame`)\cr data set supplied to the `data` argument of
#'   [mmrm::mmrm()] when fitting models. The supplied expression is
#'   [quoted][rlang::enquo] and must evaluate to a data frame. See **Tidy
#'   evaluation support**.
#' @param response (`numeric` or `string`)\cr the response variable. It can be a
#'   `string` identifying the name of an existing variable; otherwise, the
#'   supplied expression will be [quoted][rlang::enquo] and added to the formula
#'   as is (see **Tidy evaluation support**).
#' @param subject (`atomic` or `string`)\cr the unique subject identifier
#'   forwarded to the `subject` argument of [mmrm::cov_struct()]. Ignored if
#'   `cov_structs` is a `list`. Can be a `string` identifying an existing
#'   variable; otherwise the supplied expression will be [quoted][rlang::enquo]
#'   and turned into a `string` with [rlang::expr_deparse()] (see **Tidy
#'   evaluation support**).
#' @param arm (`factor` or `string`)\cr the study arm. It must be a `string` or
#'   a [name] identifying an existing variable (i.e., it cannot be a [call]). If
#'   a name, it will be [quoted][rlang::enquo] before being added to the model
#'   formula (see **Tidy evaluation support**). If it does not evaluate to a
#'   [factor] or if `control_group` is not its first [level][levels], the `data`
#'   argument will be wrapped in a [dplyr::mutate()] call that forces this to be
#'   the case.
#' @param control_group (`string`)\cr the value in `arm` denoting the control
#'   group. If necessary, `arm` will be preprocessed such that it is a factor
#'   with `control_group` as its first level.
#' @param time_observed_continuous (`numeric` or `string`)\cr the visit's
#'   *observed* time point. It must either be a `string` or a [name] identifying
#'   an existing variable (i.e., it cannot be a [call]). If a name is provided,
#'   it is [quoted][rlang::enquo] and incorporated into the model formula as is
#'   (see **Tidy evaluation support**).
#' @param df (scalar `integer`)\cr number of degrees of freedom to use to create
#'   the spline basis. Passed to the `df` argument of [time_spline_basis()].
#'   Ignored if the `spline_basis` argument is not `NULL`.
#' @param spline_basis (`basis` `matrix`)\cr a spline basis: probably a value
#'   returned by [time_spline_basis()] (which wraps [splines::ns()]). If `NULL`
#'   (the default), then the spline basis will be the result of forwarding
#'   `time_observed_continuous` and `df` to [time_spline_basis()]. See
#'   **Providing a spline basis**.
#' @param time_observed_index (`ordered` or `string`)\cr the visit index that
#'   the visit shall be associated with, based on the visit's *observed* time
#'   point. This will be passed as the `visits` argument of
#'   [mmrm::cov_struct()]. It can be a `string` identifying an existing
#'   variable; otherwise the supplied expression will be [quoted][rlang::enquo]
#'   and turned into a `string` with [rlang::expr_deparse()] (see **Tidy
#'   evaluation support**). If it does not evaluate to an `ordered` factor, it
#'   will be wrapped with [as.ordered()]. Ignored if `cov_structs` is a `list`.
#' @param time_scheduled_continuous (`numeric` or `string`)\cr the continuous
#'   time point when the visit was *scheduled* to occur. Its unique values will
#'   identify the time points at which the marginal means and other results will
#'   be calculated. It can be a `string` identifying an existing variable name;
#'   otherwise the supplied expression will be [quoted][rlang::enquo] before
#'   being evaluated (see **Tidy evaluation support**).
#' @param time_scheduled_baseline (`scalar numeric`)\cr the continuous time
#'   point when baseline was *scheduled* to occur. Defaults to 0.
#' @param time_scheduled_label (`character` or `string`)\cr the label associated
#'   with the scheduled visit. It can be a `string` identifying an existing
#'   variable name; otherwise the supplied expression will be
#'   [quoted][rlang::enquo] before being evaluated (see **Tidy evaluation
#'   support**).
#' @param covariates (`formula`)\cr formula containing additional terms that
#'   should be added to the `mmrm` model. Defaults to `~ 1`, in which no
#'   additional terms will be added. Must not have a left side. Cannot contain
#'   `.`. To specify that the model shall not have an intercept, use include `+
#'   0` or `- 1` in this formula.
#' @param average_nuisance (`flag`)\cr flag indicating whether the names of the
#'   terms in `covariates` should be supplied as the `nuisance` argument to
#'   [emmeans::emmeans()]. This results in treating all the covariates as
#'   nuisance parameters and averaging over them when calculating the reference
#'   grid to estimate marginal means. See [emmeans::ref_grid()] for details and
#'   limitations.
#' @param cov_structs (`character` or `list`)\cr either a `list` of unique
#'   [`cov_struct`][mmrm::cov_struct] objects or a `character` vector of one or
#'   more of the covariance structure abbreviations as described in
#'   [mmrm::cov_types()]. These covariance structures will be attempted in order
#'   until one of them achieves a converging model fit. Defaults to `c("us",
#'   "toeph", "ar1h", "csh", "cs")`.
#' @param cov_struct_group (`atomic` or `string`)\cr optional grouping variable
#'   to be passed to the `group` argument of [mmrm::cov_struct()]. It can be a
#'   `string` identifying an existing variable name; otherwise the supplied
#'   expression will be [quoted][rlang::enquo] and turned into a `string` with
#'   [rlang::expr_deparse()] (see **Tidy evaluation support**). Ignored if
#'   `cov_structs` is a `list`. Defaults to `NULL`, in which case no grouping
#'   variable will be used.
#' @param mmrm_args (named `list`)\cr arguments to be passed to [mmrm::mmrm()].
#'   If any elements have the names `formula`, `data`, or `covariance` they will
#'   be ignored. An element named `vcov` will also be ignored unless fitting a
#'   model with an unstructured covariance. Defaults to `list(method =
#'   "Satterthwaite")`.
#' @param emmeans_args (named `list`)\cr arguments to be passed to
#'   [emmeans::emmeans()]. If any elements have the names `object` `specs`, or
#'   `at` they will be ignored. If `average_nuisance = TRUE`, any element named
#'   `nuisance` will be ignored. Any elements named `params` may be ignored.
#'   Defaults to `list(nesting = NULL)`.
#' @param conf.level (scalar `numeric`)\cr confidence level for the calculation
#'   of p-values. Defaults to `0.95`.
#' @param change_in_bl_contrast_args,treatment_effect_contrast_args (named
#'   `list`)\cr arguments to be passed to [emmeans::contrast()] when calculating
#'   the change from baseline and treatment effect results. If any elements have
#'   the names `object` or `method` they will be ignored. Defaults to
#'   `list(adjust = "none")`.
#' @param confint_args (named `list`)\cr arguments to be passed to
#'   [stats::confint()] when calculating confidence intervals for change in
#'   baseline and treatment effect. If any element has the name `object` it will
#'   be ignored. Defaults to `list(level = conf.level)`.
#' @param expand_spline_terms (`flag`)\cr flag indicating whether or not to
#'   separate the cubic spline matrix into separate terms (one for each degree
#'   of freedom). Defaults to `TRUE`. See **Expanding spline terms**.
#' @param subgroup (`factor` or `string`)\cr the subgroup. It must be a `string`
#'   or a [name] identifying an existing variable (i.e., it cannot be a [call]).
#'   If a name, it will be [quoted][rlang::enquo] before being added to the
#'   model formula (see **Tidy evaluation support**). If it does not evaluate to
#'   a [factor] or if `subgroup_comparator` is not its first [level][levels],
#'   the `data` argument will be wrapped in a [dplyr::mutate()] call that forces
#'   this to be the case.
#' @param subgroup_comparator (`string`)\cr the value in `subgroup` denoting the
#'   "main" subgroup that all other subgroups should be compared to. If
#'   necessary, `subgroup` will be preprocessed such that it is a factor with
#'   `control_group` as its first level.
#' @param subgroup_interaction_test (`flag`)\cr flag indicating whether or not
#'   the subgroup interaction test should be performed. If `TRUE`, the returned
#'   value will include an `interaction` element, a data frame of results.
#'   Defaults to `TRUE`. See **Subgroup interaction test** for details.
#' @param return_models (`flag`)\cr flag indicating whether or not to return the
#'   model(s) used to calculate the results. See **Obtaining the models used**
#'   below.
#'
#' @details
#'
#' # Overview
#'
#' These functions create an [`mmrm`][mmrm::mmrm] model from the user-specified
#' arguments. They then perform a series of analyses and produce a data frame of
#' results with a unique row for each combination of `arm`,
#' `time_scheduled_continuous`, and `subgroup` (for `ncs_analysis_subgroup()`
#' only). The results include:
#'
#' 1. Basic diagnostics on the response variable
#'
#' 1. Estimated marginal means
#'
#' 1. Change from baseline
#'
#' 1. Treatment effect
#'
#' 1. Percent slowing
#'
#' # Building a model
#'
#' See the details of [ncs_mmrm_fit()] for information on how the model is
#' built.
#'
#' # Subgroup analysis
#'
#' `ncs_analysis_subgroup()` contains more analyses and results than
#' `ncs_analysis()`. Whereas the latter produces a data frame by default, the
#' former produces a list of data frames.
#'
#' ## Treatment effects
#'
#' The treatment effect is calculated twice: once *between* subgroups (examining
#' the differences between the subgroups within each study arm) and once
#' *within* subgroups (examining the differences between the study arms within
#' each subgroup). The main results table is effectively returned twice as both
#' the `between` element and the `within` element. These elements' treatment
#' effect values differ, and only the `within` element contains the percent
#' slowing analysis results.
#'
#' ## Type-III ANOVA
#'
#' The subgroup analyses include a type-III analysis of variance (ANOVA) on the
#' main analysis model's terms, using a Chi-squared test statistic. This is
#' accomplished via the `mmrm` method for [car::Anova()]. The results are
#' included in the returned value as the `type3` element. See
#' `vignette("hypothesis_testing", "mmrm")` for details on the type-III ANOVA.
#'
#' ## Subgroup interaction test
#'
#' When `subgroup_interaction_test = TRUE`, the function runs an ANOVA to
#' compare a maximum-likelihood-estimated (ML) version of the original model to
#' a reduced version. This happens as follows:
#'
#' 1. The original analysis model is refit with `reml = FALSE` if it was
#' originally created with `reml = TRUE`. This may be dubbed the "full" model.
#'
#' 1. A reduced version of the "full" model is created, removing the
#' second-order interaction term (see the **`arm` and `subgroup` terms** section
#' above). This may be dubbed the "reduced" model.
#'
#' 1. The "full" and "reduced" models are compared using the `mmrm` method of
#' `stats::anova()`.
#'
#' 1. The results are processed into a table and added to the returned value as
#' the `interaction` element.
#'
#' # Returning the models used
#'
#' The model(s) used to conduct the analyses can be obtained by setting
#' `return_models = TRUE`.
#'
#' For `ncs_analysis()`, the analysis model will be included as the
#' `splinetrials_analysis_model` [attr]ibute of the returned value.
#'
#' For `ncs_analysis_subgroup()`, the analysis model is added to the returned
#' value as the `analysis_model` element. Furthermore, if
#' `subgroup_interaction_test = TRUE`, the "full" and "reduced" models will be
#' included in the returned value as the elements `full` and `reduced` (see
#' **Subgroup interaction test** above for details).
#'
#' @returns For `ncs_analysis()`, see [`splinetrials_analysis-class`]. For
#'   `ncs_analysis_subgroup()`, see [`splinetrials_subgroup_analysis-class`].
#'
#' @export
#'
#' @examplesIf interactive()
#' # Create a usable data set out of mmrm::fev_data
#' fev_mod <- mmrm::fev_data
#' fev_mod$VISITN <- fev_mod$VISITN * 10
#' fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
#' fev_mod$obs_visit_index <- round(fev_mod$time_cont)
#'
#' # Without subgroup:
#' ncs_analysis(
#'   data = fev_mod,
#'   response = FEV1,
#'   subject = USUBJID,
#'   arm = ARMCD,
#'   control_group = "PBO",
#'   time_observed_continuous = time_cont,
#'   df = 2,
#'   time_observed_index = obs_visit_index,
#'   time_scheduled_continuous = VISITN,
#'   time_scheduled_baseline = 10,
#'   time_scheduled_label = AVISIT,
#'   covariates = ~ FEV1_BL + RACE,
#'   cov_structs = c("ar1", "us")
#' )
#'
#' # With subgroup:
#' ncs_analysis_subgroup(
#'   data = fev_mod,
#'   response = FEV1,
#'   subject = USUBJID,
#'   arm = ARMCD,
#'   control_group = "PBO",
#'   subgroup = SEX,
#'   subgroup_comparator = "Male",
#'   time_observed_continuous = time_cont,
#'   df = 2,
#'   time_observed_index = obs_visit_index,
#'   time_scheduled_continuous = VISITN,
#'   time_scheduled_baseline = 10,
#'   time_scheduled_label = AVISIT,
#'   covariates = ~ FEV1_BL + RACE,
#'   cov_structs = c("ar1", "us")
#' )
ncs_analysis_subgroup <- function(data,
                                   response = "response",
                                   subject = "subject",
                                   arm = "arm",
                                   control_group,
                                   subgroup = "subgroup",
                                   subgroup_comparator = "subgroup1",
                                   time_observed_continuous =
                                     "time_observed_continuous",
                                   df = 2,
                                   spline_basis = NULL,
                                   time_observed_index = "time_observed_index",
                                   time_scheduled_continuous =
                                     "time_scheduled_continuous",
                                   time_scheduled_baseline = 0,
                                   time_scheduled_label =
                                     "time_scheduled_label",
                                   covariates = ~ 1,
                                   cov_structs =
                                     c("us", "toeph", "ar1h", "csh", "cs"),
                                   cov_struct_group = NULL,
                                   mmrm_args = list(method = "Satterthwaite"),
                                   emmeans_args = list(nesting = NULL),
                                   average_nuisance = TRUE,
                                   conf.level = 0.95,
                                   change_in_bl_contrast_args =
                                     list(adjust = "none"),
                                   treatment_effect_contrast_args =
                                     list(adjust = "none"),
                                   confint_args = list(level = conf.level),
                                   subgroup_interaction_test = TRUE,
                                   return_models = FALSE,
                                   expand_spline_terms = TRUE) {
  caller_env <- rlang::caller_env()
  base_results <-
    splinetrials_analysis_base(
      data = {{data}},
      response = {{response}},
      subject = {{subject}},
      arm = {{arm}},
      control_group = control_group,
      subgroup = {{subgroup}},
      subgroup_comparator = subgroup_comparator,
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

  # Merge the response summary statistics, the marginal means table, and the
  # change from baseline table.
  between_within_table_base <-
    Reduce(
      join_subgroup_tbls,
      base_results[c("response_stats_tbl", "emmeans_tbl", "change_from_bl_tbl")]
    )

  # Merge the "within" treatment effect table and the percent slowing table.
  base_results[["within_te_tbl"]] <-
    join_subgroup_tbls(
      base_results[["within_te_tbl"]],
      base_results[["percent_slowing_tbl"]]
    )

  # Prepare the "between" and "within" tables
  between <-
    prep_between_within_tbl(
      between_within_table_base,
      base_results[["between_te_tbl"]],
      type = "between"
    )
  within <-
    prep_between_within_tbl(
      between_within_table_base,
      base_results[["within_te_tbl"]],
      type = "within"
    )

  type3 <- prepare_type3_anova_tbl(base_results[["fit"]])

  out <- list(between = between, within = within, type3 = type3)

  if (subgroup_interaction_test) {
    call <- mmrm::component(base_results[["fit"]], "call")

    # If the original analysis model was estimated with restricted maximum
    # likelihood, re-evaluate it with maximum likelihood. Otherwise, just copy
    # the original analysis model.
    if (mmrm::component(base_results[["fit"]], "reml")) {
      call[["reml"]] <- FALSE
      full <- eval(call, caller_env)
    } else {
      full <- base_results[["fit"]]
    }

    # Modify the formula of the mmrm::mmrm() call, this time specifying that we
    # want the "reduced" subgroup model.
    call[["formula"]] <-
      rlang::exec(
        prepare_formula,
        type = "subgroup_reduced",
        !!!base_results[["formula_parts"]]
      )
    reduced <- eval(call, caller_env)
    out[["interaction"]] <- prepare_anova(reduced, full)
  }

  out <-
    lapply(out, append_correlation_optimizer_cols, fit = base_results[["fit"]])

  if (return_models) {
    out[["analysis_model"]] <- base_results[["fit"]]
    if (subgroup_interaction_test) {
      out[["full_model"]] <- full
      out[["reduced_model"]] <- reduced
    }
  }

  class(out) <- "splinetrials_subgroup_analysis"

  out
}



# We saved this specific left join as its own function since it is performed
# multiple times.
join_subgroup_tbls <- function(x,
                               y,
                               join = dplyr::left_join,
                               by = c("arm", "time_num", "subgroup"),
                               ...) {
  join(x, y, by = by, ...)
}




# Combines the already combined response summary statistics table, estimated
# marginal means table, and change-from-baseline table with the treatment effect
# table. It is sorted in a different order depending on whether it's the
# "between" or "within" table.
prep_between_within_tbl <- function(table_base,
                                    te_tbl,
                                    type = c("between", "within")) {
  out <- join_subgroup_tbls(table_base, te_tbl)

  sort_exprs <-
    switch(
      type,
      between = alist(.data[["time_num"]], as.character(.data[["arm"]]),
                      as.character(.data[["subgroup"]])),
      within = alist(as.character(.data[["subgroup"]]),
                     as.character(.data[["arm"]]), .data[["time_num"]])
    )

  out <- dplyr::arrange(out, !!!sort_exprs)

  out[["time_num"]] <- NULL

  out
}



# Run a type-III ANOVA on the analysis model and put its results into a table.
prepare_type3_anova_tbl <- function(fit) {
  out <- car::Anova(fit, type = "III", test.statistic = "Chisq")
  out <- dplyr::as_tibble(as.data.frame(out), rownames = "effect")
  out <- out[out[["effect"]] != "(Intercept)", , drop = FALSE]
  out <-
    dplyr::select(
      out,
      "effect",
      chisquare_test_statistic = "Chisq",
      df = "Df",
      p_value = "Pr(>Chisq)"
    )
  out
}




# Run an ANOVA on the reduced and full models and make a table from its output.
prepare_anova <- function(reduced_fit, full_fit) {

  out <- stats::anova(reduced_fit, full_fit, test = TRUE, refit = FALSE)
  out <-
    dplyr::mutate(
      out,
      model = c("reduced model", "full model"),
      aic = .data[["AIC"]],
      bic = .data[["BIC"]],
      loglik = .data[["logLik"]],
      "-2*log(l)" = -2 * .data[["logLik"]],
      test_statistic = 2 * .data[["log_likelihood_ratio"]],
      df = c(NA, diff(.data[["df"]])),
      .data[["p_value"]],
      .keep = "none",
      .before = "df"
    )

  out
}




#' `splinetrials_subgroup_analysis` object
#'
#' `ncs_analysis_subgroup()` returns an object of class
#' `splinetrials_subgroup_analysis`: a named [list] with three to seven elements.
#'
#' @details
#'
#' # `between` and `within`
#'
#' These are each [`tibbles`][dplyr::tibble], and they share many of the same
#' columns and values but are sorted in a different order. Each contains one row
#' per unique combination of `arm`, `time_scheduled_label`, and `subgroup` found
#' in the `data` (see the arguments of [ncs_analysis_subgroup()]). The values
#' in columns `arm` through `change_p_value` as well as `correlation` and
#' `optimizer` are identical. The two tables' treatment effect analysis results
#' columns differ in name and content, with `between`'s columns bearing the
#' prefix `diff_subgroup_` and `within`'s columns bearing the prefix `diff_arm_`
#' (see the **Treatment effects** section of [ncs_analysis_subgroup()]).
#' Lastly, only `within` contains the percent slowing analysis results.
#'
#' ## `between`
#'
#' A 30-column [`tibble`][dplyr::tibble] sorted by `time`, then by `arm`, then
#' by `subgroup`.
#'
#' Columns:
#'
#' 1. `arm`: values of `data[[arm]]`.
#'
#' 1. `time`: values of `data[[time_scheduled_label]]`.
#'
#' 1. `subgroup`: values of `data[[subgroup]]`.
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
#' 1. `response_df`: degrees of freedom used to calculate the confidence
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
#' 1. `diff_subgroup_est`: treatment effect of `subgroup` within `arm`.
#'
#' 1. `diff_subgroup_se`: standard error of `diff_subgroup_est`.
#'
#' 1. `diff_subgroup_df`: degrees of freedom used for calculating the confidence
#' interval for and testing the significance of `diff_subgroup_est`.
#'
#' 1. `diff_subgroup_lower`: lower bound of confidence interval for
#' `diff_subgroup_est`.
#'
#' 1. `diff_subgroup_upper`: upper bound of confidence interval for
#' `diff_subgroup_est`.
#'
#' 1. `diff_subgroup_test_statistic`: test statistic measuring the significance
#' of `diff_subgroup_est`.
#'
#' 1. `diff_subgroup_p_value`: p-value for the significance of
#' `diff_subgroup_est`.
#'
#' 1. `correlation`: the covariance structure of the analysis model. This is the
#' same value repeated for each row.
#'
#' 1. `optimizer`: invariably `mmrm+tmb` to indicate that [mmrm::mmrm()] (which
#' uses the `TMB` package) was used to fit the model.
#'
#' # `within`
#'
#' A 33-column [`tibble`][dplyr::tibble] sorted by `subgroup`, then by `arm`,
#' then by `time`.
#'
#' Columns:
#'
#' 1. `arm`: values of `data[[arm]]`.
#'
#' 1. `time`: values of `data[[time_scheduled_label]]`.
#'
#' 1. `subgroup`: values of `data[[subgroup]]`.
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
#' 1. `change_df`: degrees of freedom for calculating the confidence interval
#' for and estimating the significance of `change_est`.
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
#' 1. `diff_arm_est`: treatment effect of `arm` within `subgroup`.
#'
#' 1. `diff_arm_se`: standard error of `diff_arm_est`.
#'
#' 1. `diff_arm_df`: degrees of freedom for calculating the confidence interval
#' for and testing the significance of `diff_arm_est`.
#'
#' 1. `diff_arm_lower`: lower bound of confidence interval for `diff_arm_est`.
#'
#' 1. `diff_arm_upper`: upper bound of confidence interval for `diff_arm_est`.
#'
#' 1. `diff_arm_test_statistic`: test statistic measuring the significance of
#' `diff_arm_est`.
#'
#' 1. `diff_arm_p_value`: p-value for the significance of `diff_arm_est`.
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
#' # `type3`
#'
#' A [`tibble`][dplyr::tibble] with a row for each term in the model (not
#' counting any intercepts). Contains the following six columns:
#'
#' 1. `effect`: the name of the model term.
#'
#' 1. `chisquare_test_statistic`: the Chi-squared test statistic measuring the
#' significance of the model term.
#'
#' 1. `df`: the degrees of freedom used for testing the significance of the
#' model term.
#'
#' 1. `p_value`: the p-value for the significance of the model term.
#'
#' 1. `correlation`: the covariance structure of the analysis model. This is the
#' same value repeated for each row.
#'
#' 1. `optimizer`: invariably `mmrm+tmb` to indicate that [mmrm::mmrm()] (which
#' uses the `TMB` package) was used to fit the model.
#'
#' # `interaction`
#'
#' This element is only present if `subgroup_interaction_test = TRUE`.
#'
#' A 2 by 10 data frame with class `anova.mmrm`. The first row represents the
#' "reduced" model and the second row represents the "full" model. The columns
#' are as follows:
#'
#' 1. `model`: `c("reduced model", "full model")`, identifying the model
#' associated with each row.
#'
#' 1. `aic`: the [AIC] of the model.
#'
#' 1. `bic`: the [BIC] of the model.
#'
#' 1. `loglik`: the [log likelihood][stats::logLik] of the model.
#'
#' 1. `-2*log(l)`: equal to `-2 * loglik`.
#'
#' 1. `test_statistic`: the test statistic used for testing the significance of
#' the second-order interaction term(s) between the spline time, `subgroup`, and
#' `arm`. This value is the second element of the column; the first element is
#' always a missing value.
#'
#' 1. `df`: the degrees of freedom used for testing the significance of the
#' second-order interaction term(s) between the spline term, `subgroup`, and
#' `arm`. This value is the second element of the column; the first element is
#' always a missing value.
#'
#' 1. `p_value`: the p-value for the significance of the second-order
#' interaction term(s) between the spline term, `subgroup`, and `arm`. This
#' value is the second element of the column; the first element is always a
#' missing value.
#'
#' 1. `correlation`: the covariance structure of the analysis model. This is the
#' same value repeated for each row.
#'
#' 1. `optimizer`: invariably `mmrm+tmb` to indicate that [mmrm::mmrm()] (which
#' uses the `TMB` package) was used to fit the model.
#'
#' # `analysis_model`
#'
#' This element is only present if `return_models = TRUE`.
#'
#' An [`mmrm`][mmrm::mmrm] object: the fitted model used to perform analyses
#' that produced the `between`, `within`, and `type3` results.
#'
#' # `full` and `reduced`
#'
#' These elements are only present if `subgroup_interaction_test = TRUE` and
#' `return_models = TRUE`.
#'
#' Both are [`mmrm`][mmrm::mmrm] objects: the two maximum-likelihood-estimated
#' models used to perform the subgroup interaction test whose results are in the
#' `interaction` element. See the **Subgroup interaction test** section of
#' [ncs_analysis_subgroup()].
#'
#' @seealso The function [ncs_analysis_subgroup()], which produces objects of
#'   this class.
#'
#' @docType class
#' @name splinetrials_subgroup_analysis-class
NULL
