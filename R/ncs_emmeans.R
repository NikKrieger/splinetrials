
#' Estimate Marginal Means for a Natural Cubic Splines Analysis
#'
#' @description This is wrapper around [emmeans::emmeans()] for a natural cubic
#'   splines analysis in which there is a continuous time variable, a study arm,
#'   and (optionally) a subgroup variable.
#'
#' @inheritParams ncs_analysis_subgroup
#' @param fit (`mmrm`)\cr an `mmrm` object whose terms include the variables
#'   supplied to `observed_time`, `scheduled_time`, `arm`, and (optionally)
#'   `subgroup`.
#' @param data (`data frame`)\cr a data frame on which to estimate marginal
#'   means. Defaults to `fit[["data"]]`.
#' @param observed_time (`string`)\cr string specifying the *observed*
#'   continuous time variable in both `fit` and in `data`.
#' @param scheduled_time (`string`)\cr string specifying the *scheduled*
#'   continuous time variable in both `fit` and in `data`. Ignored if
#'   `scheduled_time_spec` is provided.
#' @param arm (`string`)\cr string specifying the study arm variable in both
#'   `fit` and in `data`.
#' @param subgroup (`string`)\cr string specifying the subgroup variable in both
#'   `fit` and in `data`.
#' @param emmeans_args,... (named `list`)\cr arguments to be passed to
#'   [emmeans::emmeans()]. If any elements have the names `object`, `specs`, or
#'   `at` they will be ignored. If `average_nuisance = TRUE`, any element named
#'   `nuisance` will be ignored. Any elements named `params` may be ignored.
#'   `emmeans_args` defaults to `list(nesting = NULL)`. Arguments named in
#'   `emmeans_args` supersede any named arguments in `...`.
#' @param scheduled_time_spec (`numeric`)\cr vector of unique, non-missing time
#'   points on which to calculate marginal means. Defaults to
#'   `sort(unique(data[[scheduled_time]]))`.
#' @param arm_spec (`character`)\cr vector of unique study arm values on which
#'   to calculate marginal means. Defaults to
#'   `as.character(sort(unique(data[[arm]])))`.
#' @param subgroup_spec vector of unique subgroup values on which to calculate
#'   marginal means. Ignored if `subgroup` is `NULL`. Defaults to
#'   `as.character(sort(unique(data[[subgroup]])))`.
#' @param .__caller_env (`environment`)\cr the environment from which this
#'   function was called. Defaults to [rlang::caller_env()].
#'
#' @returns An object of class \code{\link[emmeans:emmGrid-class]{emmGrid}}: the
#'   result of [emmeans::emmeans()]. Note that for a result `result`, the
#'   elements `result@model.info$nesting` and `result@misc$display` are removed.
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
#' fit <-
#'   ncs_mmrm_fit(
#'     data = fev_mod,
#'     type = "subgroup_full",
#'     response = FEV1,
#'     subject = USUBJID,
#'     cov_structs = c("ar1", "us"),
#'     time_observed_continuous = time_cont,
#'     df = 2,
#'     time_observed_index = obs_visit_index,
#'     arm = ARMCD,
#'     control_group = "PBO",
#'     subgroup = SEX,
#'     subgroup_comparator = "Male",
#'     covariates = ~ FEV1_BL + RACE
#'   )
#'
#' ncs_emmeans(
#'   fit = fit,
#'   observed_time = "time_cont",
#'   scheduled_time = "VISITN",
#'   arm = "ARMCD",
#'   subgroup = "SEX"
#' )
ncs_emmeans <- function(fit,
                        data = fit[["data"]],

                        observed_time = NULL,
                        scheduled_time = NULL,
                        arm = NULL,
                        subgroup = NULL,

                        average_nuisance = TRUE,
                        emmeans_args = list(nesting = NULL),
                        ...,

                        scheduled_time_spec =
                          sort(unique(data[[scheduled_time]])),
                        arm_spec = as.character(sort(unique(data[[arm]]))),
                        subgroup_spec =
                          as.character(sort(unique(data[[subgroup]]))),
                        .__caller_env = rlang::caller_env()) {
  validate_arg_list(emmeans_args, call = .__caller_env)
  validate_flag(average_nuisance, call = .__caller_env)
  validate_single_string(arm, data = data)
  validate_single_string(observed_time, data = data)
  if (!is.null(subgroup)) {
    validate_single_string(subgroup, data = data)
  }

  # Adds columns to data from the environment because of the limitations of
  # emmeans::emmeans().
  fit[["data"]] <- data <- construct_full_data_set(fit, call = .__caller_env)

  if (is.null(scheduled_time)) {
    if (missing(scheduled_time_spec)) {
      cli::cli_abort(c(
        "If {.arg scheduled_time} is {.code NULL} then {.arg scheduled_time_spec} cannot be left as the default and requires a value.",
        "x" = "{.arg scheduled_time} is {.code NULL} and {.arg scheduled_time_spec} was not provided."
      ))
    }
    validate_numeric(
      scheduled_time_spec,
      min_length = 1L,
      NA_ok = FALSE,
      duplicates_ok = FALSE,
      call = .__caller_env
    )
  } else {
    validate_single_string(scheduled_time, data = data)
  }

  arm_spec <-
    validate_emmeans_spec(
      spec = arm_spec,
      var = arm,
      data = data,
      call = .__caller_env
    )

  # Prepare the "at" and "specs" emmeans::emmeans() arguments.
  at <-
    if (is.null(subgroup)) {
      list(arm_spec, scheduled_time_spec)
    } else {
      subgroup_spec <-
        validate_emmeans_spec(
          spec = subgroup_spec,
          var = subgroup,
          data = data,
          call = .__caller_env
        )
      list(arm_spec, scheduled_time_spec, subgroup_spec)
    }
  names(at) <- specs <- c(arm, observed_time, subgroup)


  # Populate the params argument with any emmeans::.all.vars() results that are
  # not a column name in data.
  all_vars <- emmeans::.all.vars(fit[["formula_parts"]][["full_formula"]])
  params <- setdiff(all_vars, colnames(data))
  if (length(params)) {
    emmeans_args[["params"]] <- params
  }

  if (average_nuisance) {
    # Specify as nuisance terms all the covariates other than time, arm, and
    # subgroup.
    covariates <- setdiff(fit[["formula_parts"]][["model_var"]], specs)
    if (length(covariates)) emmeans_args[["nuisance"]] <- covariates
  }

  emmeans_args <-
    rlang::dots_list(
      object = fit,
      data = data,
      specs = specs,
      at = at,
      !!!emmeans_args,
      ...,
      .homonyms = "first"
    )

  out <- do.call(emmeans::emmeans, emmeans_args)

  # This was done in the old ncs so we do it here.
  tryCatch(out@model.info$nesting <- NULL, error = identity)
  tryCatch(out@misc$display <- NULL, error = identity)

  out
}




# This function accepts an mmrm model fit and a data set. It reads the
# "full_formula" of the mmrm fit (which contains all the variables in the model
# including those that are used to specify the covariance matrix) and finds the
# variables that aren't in data. It then iterates through those variables, finds
# the ones with a length the same as nrow(data), and adds these variables to
# data.
construct_full_data_set <- function(fit, call = rlang::caller_env()) {
  data <- fit[["data"]]

  # The last column of fit[["data"]] is the weights column. Save this column's
  # name and drop it from data.
  weights_name <- names(data)[ncol(data)]
  data <- data[, -ncol(data), drop = FALSE]

  full_formula <- fit[["formula_parts"]][["full_formula"]]
  all_vars <- all.vars(full_formula)
  env <- environment(full_formula)
  nrow <- nrow(data)

  # names in full_formula that are not column names of data.
  env_vars <- setdiff(all_vars, colnames(data))

  for (var_name in env_vars) {
    # Evaluate the name. If the object is a vector with length the same as
    # nrow(data), add it as a column of data.
    var <- tryCatch(get(var_name, envir = env), error = function(e) NULL)
    if (rlang::is_vector(var, n = nrow)) data[[var_name]] <- var
  }

  # We can't end up having a variable whose name matches the name of the weight
  # variable.
  if (any(colnames(data) == weights_name)) {
    cli::cli_abort(c(
      "Variables used in the model cannot have the same name as the weights variable.",
      "x" = "One of the variables in the model is named {.code {weights_name}}.",
      "i" = "Rename this variable."
    ), call = call)
  }

  # Add the weight variable back.
  data[[weights_name]] <- fit[["data"]][[weights_name]]

  data
}
