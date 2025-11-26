#' Create a Mixed Model with Repeated Measures Using Natural Cubic Splines.
#'
#' @description Builds an [`mmrm`][mmrm::mmrm] model that includes a study arm,
#'   optionally a subgroup, and natural cubic splines applied to a continuous
#'   time variable. A wrapper around [mmrm::mmrm()].
#'
#'   Constructs a call to [mmrm::mmrm()] for ncs analysis. Implements natural
#'   cubic splines for the continuous time variable. Attempts a sequence of
#'   covariance structures in order until one of them successfully converges.
#'   Title
#'
#' @inheritParams ncs_analysis_subgroup
#' @param type (`string`)\cr one of `"basic"`, `"subgroup_full"`, or
#'   `"subgroup_reduced"`.
#' @param ... additional arguments to be passed to [mmrm::mmrm()]. If any
#'   elements have the names `formula`, `data`, or `covariance` they will be
#'   ignored. An element named `vcov` will also be ignored unless fitting a
#'   model with an unstructured covariance. Defaults to `list(method =
#'   "Satterthwaite")`. Arguments named in `mmrm_args` supersede any named
#'   arguments in `...`.
#'
#' @details
#'
#' # Providing a spline basis
#'
#' This function's `spline_basis` argument was designed with [splines::ns()] in
#' mind, which creates a matrix object with classes `basis` and `matrix` as well
#' as multiple attributes. In theory, `spline_basis` does not have to be a
#' `matrix`; however, it still must have a [stats::predict()] method wherein
#' `stats::predict(spline_basis, data[[time_observed_continuous]])` produces an
#' object that can serve as a term in the model.
#'
#' # Covariance structures
#'
#' The user specifies covariance structure *candidates* via the `cov_structs`
#' argument. These structures will be attempted in order until a model converges
#' successfully.
#'
#' When any covariance structure other than `"us"` (heterogeneous unstructured)
#' is used, `"Empirical-Bias-Reduced"` is passed to [mmrm::mmrm()] as the `vcov`
#' argument (see [mmrm::mmrm_control()]).
#'
#' When fitting models, these analysis functions specify the covariance
#' structure through the `covariance` argument of [mmrm::mmrm()].
#'
#' # Building the model formula
#'
#' These analysis functions automatically build the model formula from its
#' arguments. The user cannot remove any of these auto-generated terms, but
#' terms can be added via the `covariates` argument.
#'
#' ## Time spline terms
#'
#' Natural cubic splines will be applied to the `time_observed_continuous`
#' variable in `data`. These splines will be constructed according to the
#' user-specified `spline_basis`. A custom `spline_fn()` is constructed under
#' the hood that accepts `time_observed_continuous` and produces a spline matrix
#' based on the `spline_basis`. Thus, the model formula includes a time spline
#' term resembling `spline_fn(time_observed_continuous)`.
#'
#' ## `arm` and `subgroup` terms
#'
#' All generated models include an interaction term between the time spline term
#' and the study `arm` term, but `arm` is not included as a main effect by
#' default. If this is desired, use the `covariates` argument (e.g., specify
#' `covariates = ~ arm`).
#'
#' Concerning `ncs_analysis_subgroup()`, the `subgroup` variable is included as
#' a main effect, and its interaction with the time spline is also included.
#' Furthermore, the second-order interaction term between the time spline,
#' `subgroup`, and `arm` is also included for the main analysis model and the
#' "full" model (when `subgroup_interaction_test = TRUE`; see **Subgroup
#' interaction test** below).
#'
#' ## Adding terms with `covariates`
#'
#' The user can specify additional terms through the `covariates` argument,
#' which must be a formula.
#'
#' The user cannot specify the covariance structure with this argument. See the
#' **Covariance structures** section above.
#'
#' The user can remove the intercept from the model by including `0` as a term
#' in `covariates`.
#'
#' ## Model formula templates
#'
#' The model formulas that the analysis functions construct will take the form
#' of the formula templates below.
#'
#' ### `ncs_analysis()` (i.e., no subgroup)
#'
#' ```
#' response ~
#'   spline_fn(time_observed_continuous) +
#'   spline_fn(time_observed_continuous):arm {+
#'   covariates}
#' ```
#'
#' ### `ncs_analysis_subgroup()`
#'
#' Main analysis model and "full" model:
#'
#' ```
#' response ~
#'   spline_fn(time_observed_continuous) +
#'   subgroup +
#'   spline_fn(time_observed_continuous):subgroup +
#'   spline_fn(time_observed_continuous):arm +
#'   spline_fn(time_observed_continuous):subgroup:arm {+
#'   covariates}
#' ```
#'
#' "reduced" model:
#'
#' ```
#' response ~
#'   spline_fn(time_observed_continuous) +
#'   subgroup +
#'   spline_fn(time_observed_continuous):subgroup +
#'   spline_fn(time_observed_continuous):arm {+
#'   covariates}
#' ```
#'
#' ## Expanding spline terms
#'
#' When `expand_spline_terms = TRUE` and `spline_basis` has at least two
#' [dim]ensions (e.g., if it is a matrix, which is typical), the spline term
#' will be split into multiple terms: one for each of its columns.
#'
#' For instance, if the user specifies a `spline_basis` with 3 degrees of
#' freedom, the above no-subgroup model formula template would become:
#'
#' ```
#' response ~
#'   spline_fn(time_observed_continuous)[, 1] +
#'   spline_fn(time_observed_continuous)[, 2] +
#'   spline_fn(time_observed_continuous)[, 3] +
#'   spline_fn(time_observed_continuous)[, 1]:arm +
#'   spline_fn(time_observed_continuous)[, 2]:arm +
#'   spline_fn(time_observed_continuous)[, 3]:arm {+
#'   covariates}
#' ```
#'
#' @returns An `mmrm` object created by [mmrm::mmrm()].
#'
#' @export
#'
#' @examples
#' # Create a usable data set out of mmrm::fev_data
#' fev_mod <- mmrm::fev_data
#' fev_mod$VISITN <- fev_mod$VISITN * 10
#' fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
#' fev_mod$obs_visit_index <- round(fev_mod$time_cont)
#'
#' # Example without subgroup:
#' ncs_mmrm_fit(
#'   data = fev_mod,
#'   type = "basic",
#'   response = FEV1,
#'   subject = USUBJID,
#'   cov_structs = c("ar1", "us"),
#'   time_observed_continuous = time_cont,
#'   df = 2,
#'   time_observed_index = obs_visit_index,
#'   time_scheduled_continuous = VISITN,
#'   arm = ARMCD,
#'   control_group = "PBO",
#'   covariates = ~ FEV1_BL + RACE
#' )
#'
#' # Example with subgroup:
#' ncs_mmrm_fit(
#'   data = fev_mod,
#'   type = "subgroup_full",
#'   response = FEV1,
#'   subject = USUBJID,
#'   cov_structs = c("ar1", "us"),
#'   time_observed_continuous = time_cont,
#'   df = 2,
#'   time_observed_index = obs_visit_index,
#'   time_scheduled_continuous = VISITN,
#'   arm = ARMCD,
#'   control_group = "PBO",
#'   subgroup = SEX,
#'   subgroup_comparator = "Male",
#'   covariates = ~ FEV1_BL + RACE
#' )
ncs_mmrm_fit <- function(data,
                         type = c("basic", "subgroup_full", "subgroup_reduced"),
                         response,
                         subject,
                         cov_structs = c("us", "toeph", "ar1h", "csh", "cs"),
                         cov_struct_group = NULL,
                         time_observed_continuous,
                         df = 2,
                         spline_basis = NULL,
                         time_observed_index,
                         time_scheduled_continuous = NULL,
                         arm = NULL,
                         control_group = "control",
                         subgroup = NULL,
                         subgroup_comparator = NULL,
                         covariates = ~ 1,
                         expand_spline_terms = TRUE,
                         mmrm_args = list(method = "Satterthwaite"),
                         ...) {
  type <- match.arg(type)
  validate_single_string(control_group)
  caller_env <- rlang::caller_env()
  data_expr <- rlang::quo_squash(rlang::enquo(data))
  response <- validate_user_var({{response}}, call = caller_env)
  arm <-
    validate_user_var(
      {{arm}},
      require_symbol = TRUE,
      data = data,
      call = caller_env
    )
  subgroup <-
    validate_user_var(
      {{subgroup}},
      null_ok = TRUE,
      require_symbol = TRUE,
      data = data,
      call = caller_env
    )

  if (is.null(subgroup)) {
    switch(
      type,
      subgroup_full = ,
      subgroup_reduced =
        cli::cli_abort(c(
          'If {.arg subgroup} is `NULL`, {.arg type} must be {.code "basic"}.',
          "x" = '{.arg subgroup} is `NULL` and {.arg type} is {type}.'
        ), call = caller_env)
    )
  } else {
    if (type == "basic") {
      cli::cli_abort(c(
        'If {.arg subgroup} is not `NULL`, {.arg type} must be {.code "subgroup_full"} or {.code "subgroup_reduced"}.',
        "x" = '{.arg subgroup} is {subgroup} and {.code type = "basic"}.'
      ), call = caller_env)
    }
    if (!is.null(subgroup_comparator))
      validate_single_string(subgroup_comparator)
  }

  time_observed_continuous <-
    validate_user_var(
      {{time_observed_continuous}},
      require_symbol = TRUE,
      call = caller_env
    )

  data_expr <-
    prepare_data_expr(
      data_expr = data_expr,
      col_expr = arm,
      ref_value = control_group,
      data = data,
      env = caller_env
    )
  if (!is.null(subgroup)) {
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
      call = caller_env
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
      call = caller_env
    )

  out <-
    ncs_mmrm_fit_impl(
      formula_parts = formula_parts,
      type = type,
      data_expr = data_expr,
      cov_structs = cov_structs,
      mmrm_args = mmrm_args,
      ...,
      caller_env = caller_env
    )

  out
}






# Constructs a call to [mmrm::mmrm()] and attempts the list of covariance
# structures in order until one of them successfully converges.
ncs_mmrm_fit_impl <- function(formula_parts,
                              type =
                                c("basic", "subgroup_full", "subgroup_reduced"),
                              data_expr,
                              cov_structs,
                              mmrm_args = list(),
                              ...,
                              caller_env = rlang::caller_env()) {
  type <- match.arg(type)

  formula <- rlang::exec(prepare_formula, type = type, !!!formula_parts)

  mmrm_args <- rlang::dots_list(!!!mmrm_args, ..., .homonyms = "first")

  mmrm_args <-
    prepare_mmrm_args(
      mmrm_args = mmrm_args,
      formula = formula,
      data_expr = data_expr,
      call = caller_env
    )

  eval_env <- environment(formula)

  for (cov_struct in cov_structs) {
    out <- fit_cov_struct(mmrm_args, cov_struct, eval_env, call = caller_env)
    if (converged(out)) break
    cli::cli_alert('The covariance structure {.code "{cov_struct$type}"} did not successfully converge.')
  }

  if (!inherits(out, "mmrm")) {
    cli::cli_abort(c(
      "At least one of the provided covariance structures must lead to a converging model.",
      "x" = "None of the covariance structure types led to a converging model."
    ), call = caller_env)
  }

  out
}




# Prepares the necessary components for an ncs model formula. Its result will be
# passed along to prepare_formula(), where it will be specified whether the
# formula should be full subgroup, reduced subgroup, or subgroup-free.
prepare_formula_parts <- function(response,
                                  spline_basis,
                                  time_observed_continuous,
                                  arm,
                                  subgroup = NULL,
                                  covariates,
                                  caller_env,
                                  expand_spline_terms,
                                  call = rlang::caller_env()) {
  # This is the user-specified "covariates" formula.
  validate_covariates(covariates, call = call)

  # Create a child environment of the caller environment. This will be the
  # enclosure of the spline function.
  spline_fn_parent_env <- new.env(parent = caller_env)
  spline_fn_parent_env[["spline_basis"]] <- spline_basis

  # Create another child environment of the caller environment. This will be the
  # environment in which calls to mmrm() are evaluated.
  mmrm_eval_env <- new.env(parent = caller_env)
  # Generate a unique name for the spline function.
  spline_fn_name <- make_unique_name("spline_fn", caller_env)
  # Create the spline function and bind it in the mmrm() evaluation environment.
  mmrm_eval_env[[spline_fn_name]] <-
    rlang::set_env(
      function(time) unclass(stats::predict(spline_basis, time)),
      new_env = spline_fn_parent_env
    )

  spline_call <-
    create_spline_call(
      spline_fn_name,
      time_observed_continuous,
      expand_spline_terms,
      spline_basis
    )

  formula_base <- create_formula_base(response, covariates, mmrm_eval_env)

  out <-
    list(
      formula_base = formula_base,
      spline_call = spline_call,
      arm = arm,
      subgroup = subgroup
    )

  out
}



# Modifies the user-specified "covariates" formula so that its lefthand side is
# the response variable and its environment is the one created in
# prepare_formula_parts().
create_formula_base <- function(response, covariates, env) {
  rlang::f_lhs(covariates) <- response
  environment(covariates) <- env
  covariates
}



# If expand_spline_terms is FALSE, this will simply be something like
# spline_fn(time_obs_cont). If expand_spline_terms is TRUE, each column of
# spline_fn(time_obs_cont) gets its own term and the whole thing is wrapped in
# parentheses, e.g.:
#
# (spline_fn(time)[, 1] + spline_fn(time)[, 2] + ...)
create_spline_call <- function(spline_fn_name,
                               time_observed_continuous,
                               expand_spline_terms,
                               spline_basis) {
  out <- call(spline_fn_name, time_observed_continuous)

  if (expand_spline_terms && NCOL(spline_basis) > 1L) {
    spline_col_indices <- as.double(seq_len(ncol(spline_basis)))
    spline_calls <-
      lapply(
        spline_col_indices,
        function(i) rlang::expr((!!out)[, !!i])
      )
    out <- Reduce(function(x, i) call("+", x, i), spline_calls)
    out <- call("(", out)
  }

  out
}




# Completes the model formula by adding the splined time terms, the subgroup
# term (if applicable) and the splined time-arm-subgroup interaction terms.
prepare_formula <- function(type =
                              c("basic", "subgroup_full", "subgroup_reduced"),
                            formula_base,
                            spline_call,
                            arm,
                            subgroup,
                            ...) {
  type <- match.arg(type)

  spline_terms_update <-
    switch(
      type,
      basic = rlang::expr(~!!spline_call + !!spline_call:!!arm + .),
      subgroup_reduced =
        rlang::expr(~!!spline_call * !!subgroup + !!spline_call:!!arm + .),
      subgroup_full =
        rlang::expr(~!!spline_call * !!subgroup + !!spline_call:!!arm +
                      !!spline_call:!!subgroup:!!arm + .)
    )

  formula <- stats::update(formula_base, spline_terms_update)

  formula
}





# Completes the call to [mmrm::mmrm()] depending on the covariance structure and
# evaluates it. Convergence errors are caught and documented, but other errors
# are allowed to cause the function to fail.
fit_cov_struct <- function(mmrm_args,
                           covariance,
                           env,
                           call = rlang::caller_env()) {

  mmrm_args[["covariance"]] <- covariance
  if (covariance[["type"]] != "us") {
    mmrm_args[["vcov"]] <- "Empirical-Bias-Reduced"
  }

  mmrm_call <- rlang::expr(mmrm::mmrm(!!!mmrm_args))

  out <-
    tryCatch(
      eval(mmrm_call, env),
      error = function(e) {
        if (any(grepl("convergence problem", e[["message"]]))) {
          return(
            structure(
              mmrm_call,
              env = env,
              converged = FALSE,
              error_message = e
            )
          )
        }
        cli::cli_abort(c(
          "Supplied arguments must not lead to an error in {.fn mmrm::mmrm()} unless it is a convergence error.",
          "x" = "A non-convergence-related error occurred in {.fn mmrm::mmrm()}:",
          rlang::set_names(e[["message"]], "x"),
          "i" = "Check that all supplied variables actually exist in {.arg data}.",
          "i" = "Check especially {.arg response}, {.arg covariates}, and {.arg mmrm_args}."
        ), call = call)
      }
    )

  out
}




# Check the convergence of a model fit. Simply returns the `converged`
# attribute.
converged <- function(fit) {
  attr(fit, "converged")
}




# This function accepts the expression that the user entered as the model data
# set. It checks to see if col_expr evaluates to a factor with ref_value as its
# first factor level. If not, data_expr is wrapped in dplyr::mutate(), modifying
# col_expr so that it will become a factor with ref_value as its first level.
prepare_data_expr <- function(data_expr, col_expr, ref_value, data, env) {
  values <- eval(col_expr, data, env)

  # If not a factor or if ref_value isn't the first factor level.
  if (
    !is.factor(values) ||
    !is.null(ref_value) && match(ref_value, levels(values), 0L) != 1L
  ) {
    # Save the name of the column.
    colname <- as.character(col_expr)
    # Make the column's expression a call to factor() with ref_value as the
    # first factor level.
    col_expr <-
      call("factor", col_expr, union(ref_value, levels(as.factor(values))))
    # Wrap data_expr in a call to dplyr::mutate() if it is not already the case.
    if (!rlang::is_call(data_expr, name = "mutate", ns = "dplyr")) {
      data_expr <- rlang::expr(dplyr::mutate(!!data_expr))
    }
    # Find the numeric index of the new argument.
    new_arg_index <- length(data_expr) + 1L
    # Add the new call to factor() as the last element of data_expr, which is
    # now guaranteed to be a call.
    data_expr[[new_arg_index]] <- col_expr
    # Add the name of the column to the newly created argument.
    names(data_expr)[new_arg_index] <- colname
  }

  data_expr
}
