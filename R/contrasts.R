
#' @rdname treatment_effect
#'
#' @export
change_from_baseline <- function(emmeans,
                                 time_observed_continuous =
                                   emmeans@roles$predictors[2],
                                 time_scheduled_baseline = 0,
                                 arm = emmeans@roles$predictors[1],
                                 subgroup =
                                   if (length(emmeans@roles$predictors) == 3)
                                     emmeans@roles$predictors[3],
                                 contrast_args = list(adjust = "none"),
                                 ...,
                                 as_tibble = FALSE,
                                 confint_args = list(level = 0.95)) {
  ncs_contrasts(
    emmeans = emmeans,
    type = "change_from_bl",
    time_observed_continuous = time_observed_continuous,
    time_scheduled_baseline = time_scheduled_baseline,
    arm = arm,
    subgroup = subgroup,
    ref_value = NULL,
    contrast_args = contrast_args,
    ...,
    as_tibble = as_tibble,
    confint_args = confint_args
  )
}


#' Calculate the Change from Baseline or Treatment Effects from Estimated
#' Marginal Means
#'
#' @description Pass [`emmeans::emmeans()`] objects (probably obtained via
#'   [ncs_emmeans()]) to [`emmeans::contrast()`] using specially constructed
#'   contrast matrices so that change from baseline and treatment effects can be
#'   calculated.
#'
#' - `change_from_baseline` calculate the change from baseline for each of the
#'   different study arms/subgroups.
#'
#' - `treatment_effect()` calculate the treatment effect for each study arm
#'   when there is no subgroup. When there is a subgroup, calculate the
#'   treatment effect *between* subgroups (examining the differences
#'   *between* the subgroups within each study arm) or *within* subgroups
#'   (examining the differences between the study arms *within* each subgroup).
#'
#' @param emmeans (`emmGrid`)\cr an object of class
#'   \code{\link[emmeans:emmGrid-class]{emmGrid}}, ideally obtained via
#'   [ncs_emmeans()], which wraps [emmeans::emmeans()].
#' @param arm,time_observed_continuous,subgroup (`string`)\cr strings
#'   identifying the study arm variable, *observed* continuous time variable,
#'   and (optionally) subgroup variable supplied to [emmeans::emmeans()],
#'   probably via [ncs_emmeans()]). If [ncs_emmeans()] was indeed used, these
#'   strings *should* be contained in the character vector
#'   `emmeans@roles$predictors` (see the default arguments).
#' @param time_scheduled_baseline (`scalar numeric`)\cr the continuous time
#'   point when baseline was *scheduled* to occur. Defaults to 0.
#' @param ref_value (`string`)\cr the value in `arm` (if `subgroup = NULL` *or*
#'   if `subgroup_type = "within"`) or the value in `subgroup` (if `subgroup` is
#'   not `NULL` *and* `subgroup_type = "between"`) denoting the control group.
#' @param subgroup_type (`string`)\cr either `"between"` or `"within"`, denoting
#'   whether to calculate the treatment effect *between* subgroups (examining
#'   the differences between the subgroups within each study arm) and once
#' *within* subgroups (examining the differences between the study arms within
#'   each subgroup).
#' @param contrast_args,... (named `list`)\cr arguments to be passed to
#'   [emmeans::contrast()]. Any arguments with the names `object` or `method`
#'   will be overwritten. Arguments in `contrast_args` override identically
#'   named arguments in `...`.
#' @param as_tibble (`flag`)\cr `TRUE` or `FALSE` indicating whether or not the
#'   results of [emmeans::contrast()] should be processed and returned as a
#'   [tibble][dplyr::tibble].
#' @param confint_args (named `list`)\cr arguments to be passed to
#'   [stats::confint()] when calculating confidence intervals. Ignored if
#'   `as_tibble = FALSE`. If `NULL`, confidence intervals will not be
#'   calculated. Defaults to `list(level = 0.95)`.
#'
#' @returns When `as_tibble = FALSE`, the value returned by
#'   [emmeans::contrast()]. If `as_tibble = TRUE`, a [tibble][dplyr::tibble]:
#'
#'   1. \{*column name will be the value of the `arm` argument*\}: the study
#'   arm.
#'
#'   1. \{*column name will be the value of the `time_observed_continuous`
#'   argument*\}: the *observed* continuous time variable.
#'
#'   1. \{*column name will be the value of the `subgroup` argument*\}: the
#'   subgroup. **Only present if `subgroup` is not `NULL`.**
#'
#'   1. `estimate`: estimate for change from baseline or treatment effect.
#'
#'   1. `SE`: standard error of `estimate`.
#'
#'   1. `df`: degrees of freedom for calculating the confidence interval for and
#'   estimating the significance of `estimate`.
#'
#'   1. `lower.CL`: lower bound of confidence interval for `estimate`. **Only
#'   present if `confint_args` is not `NULL`.**
#'
#'   1. `upper.CL`: upper bound of confidence interval for `estimate`. **Only
#'   present if `confint_args` is not `NULL`.**
#'
#'   1. `t.ratio`: test statistic measuring the significance of `estimate`.
#'
#'   1. `p.value`: p-value for the significance of `estimate`.
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
#'     time_scheduled_continuous = VISITN,
#'     arm = ARMCD,
#'     control_group = "PBO",
#'     subgroup = SEX,
#'     subgroup_comparator = "Male",
#'     covariates = ~ FEV1_BL + RACE
#'   )
#'
#' marginal_means <-
#'   ncs_emmeans(
#'     fit = fit,
#'     observed_time = "time_cont",
#'     scheduled_time = "VISITN",
#'     arm = "ARMCD",
#'     subgroup = "SEX"
#'   )
#'
#' change_from_baseline(
#'   emmeans = marginal_means,
#'   time_observed_continuous = "time_cont",
#'   time_scheduled_baseline = 10,
#'   arm = "ARMCD",
#'   subgroup = "SEX"
#' )
#'
#' # Same thing as a tibble:
#' change_from_baseline(
#'   emmeans = marginal_means,
#'   time_observed_continuous = "time_cont",
#'   time_scheduled_baseline = 10,
#'   arm = "ARMCD",
#'   subgroup = "SEX",
#'   as_tibble = TRUE
#' )
#'
#' treatment_effect(
#'   emmeans = marginal_means,
#'   time_observed_continuous = "time_cont",
#'   time_scheduled_baseline = 10,
#'   arm = "ARMCD",
#'   subgroup = "SEX",
#'   ref_value = "Male",
#'   as_tibble = TRUE
#' )
treatment_effect <- function(emmeans,
                             time_observed_continuous =
                               emmeans@roles$predictors[2],
                             time_scheduled_baseline,
                             arm = emmeans@roles$predictors[1],
                             subgroup =
                               if (length(emmeans@roles$predictors) == 3)
                                 emmeans@roles$predictors[3],
                             ref_value,
                             subgroup_type = c("between", "within"),
                             contrast_args = list(adjust = "none"),
                             ...,
                             as_tibble = FALSE,
                             confint_args = list(level = 0.95)) {
  ncs_contrasts(
    emmeans = emmeans,
    type = "treatment_effect",
    time_observed_continuous = time_observed_continuous,
    time_scheduled_baseline = time_scheduled_baseline,
    arm = arm,
    subgroup = subgroup,
    ref_value = ref_value,
    subgroup_type = subgroup_type,
    contrast_args = contrast_args,
    ...,
    as_tibble = as_tibble,
    confint_args = confint_args
  )
}




# [emmeans::contrast()] wrapper for ncs analysis. Constructs the contrast
# matrices depending on the "type" argument.
ncs_contrasts <- function(emmeans,
                          type = c("change_from_bl", "treatment_effect"),
                          time_observed_continuous =
                            emmeans@roles$predictors[2],
                          time_scheduled_baseline = 0,
                          arm = emmeans@roles$predictors[1],
                          subgroup = if (length(emmeans@roles$predictors) == 3)
                            emmeans@roles$predictors[3],
                          ref_value = NULL,
                          subgroup_type = c("between", "within"),
                          contrast_args = list(),
                          ...,
                          as_tibble = FALSE,
                          confint_args = list(level = 0.95)) {
  # The unique combinations of arm, time_observed_continuous, and subgroup (if
  # present).
  grid <- emmeans@grid[c(arm, time_observed_continuous, subgroup)]

  # Initialize the contrast matrix.
  method <- diag(nrow = nrow(grid))

  # Add row and column names.
  rownames(method) <- colnames(method) <-
    paste(
      grid[[arm]],
      grid[[time_observed_continuous]],
      if (!is.null(subgroup)) grid[[subgroup]],
      sep = "_"
    )

  # Create a row number variable so that the original order can be recovered.
  rn <- make_unique_name("rn", vec = colnames(grid))
  grid[[rn]] <- seq_len(nrow(grid))
  bl_index <- make_unique_name("bl_index", vec = colnames(grid))
  # Add a column identifying the row corresponding to the time baseline row.
  grid[[bl_index]] <-
    get_ref_indices(
      grid = grid,
      var = time_observed_continuous,
      ref_value = time_scheduled_baseline,
      grouping = c(arm, subgroup),
      row_number_colname = rn
    )

  # Two-column matrix identifying the coordinates in "method" that need a -1.
  baseline_coords_mtx <- as.matrix(grid[c(bl_index, rn)])
  method[baseline_coords_mtx] <- -1

  if (type == "treatment_effect") {
    subgroup_type <- match.arg(subgroup_type)
    if (is.null(subgroup) || subgroup_type == "within") {
      validate_ref_value(ref_value, grid = grid, var_name = arm)
      grouping <- subgroup
      var <- arm
    } else {
      validate_ref_value(ref_value, grid = grid, var_name = subgroup)
      grouping <- arm
      var <- subgroup
    }

    ref_index <- make_unique_name("ref_index", vec = colnames(grid))
    # For non-subgroup analyses and "within" subgroup analyses, add a column
    # identifying the row corresponding to the control arm.
    #
    # For "between" subgroup analyses, add a column identifying the subgroup
    # comparator row.
    grid[[ref_index]] <-
      get_ref_indices(
        grid = grid,
        var = var,
        ref_value = ref_value,
        grouping = c(time_observed_continuous, grouping),
        row_number_colname = rn
      )

    # Two-column matrix identifying the coordinates in "method" that need a -1.
    ref_coords_mtx <- as.matrix(grid[c(ref_index, rn)])
    method[ref_coords_mtx] <- -1

    # Create a new column identifying the row that is both the time baseline and
    # the arm/subgroup reference value.
    bl_ref <- make_unique_name("bl_ref", vec = colnames(grid))
    grid[[bl_ref]] <- grid[grid[[ref_index]], bl_index, drop = TRUE]

    # Two-column matrix identifying the coordinates in "method" that need  +1.
    bl_ref_mtx <- cbind(grid[[bl_ref]], grid[[rn]])
    method[bl_ref_mtx] <- +1

    # Indices of time and arm/subgroup baseline columns, which shall be removed.
    indices_to_remove <- union(grid[[bl_index]], grid[[ref_index]])
  } else {
    # Indices of time baseline columns, which shall be removed.
    indices_to_remove <- unique(grid[[bl_index]])
  }

  method <- method[, -indices_to_remove, drop = FALSE]

  # Column names of method become the names of the list.
  method <- as.list(as.data.frame(method))

  contrast_args <-
    rlang::dots_list(
      object = emmeans,
      method = method,
      !!!contrast_args,
      ...,
      .homonyms = "first"
    )

  contrasts <- do.call(emmeans::contrast, contrast_args)

  if (as_tibble) {
    out <-
      dplyr::tibble(
        # Start with the unique combinations of arm, time, and subgroup. Remove
        # rows corresponding to the time baseline and reference groups.
        grid[-indices_to_remove, c(arm, time_observed_continuous, subgroup)],
        as.data.frame(contrasts)[
          c("estimate", "SE", "df", "t.ratio", "p.value")
        ],
        .name_repair = "unique_quiet"
      )

    # Add confidence intervals.
    if (is.list(confint_args)) {
      confint_args <-
        rlang::dots_list(
          object = contrasts,
          !!!confint_args,
          .homonyms = "first"
        )
      ci <- do.call(stats::confint, confint_args)
      out <-
        dplyr::tibble(
          out[
            c(arm, time_observed_continuous, subgroup, "estimate", "SE", "df")
          ],
          ci[c("lower.CL", "upper.CL")],
          out[c("t.ratio", "p.value")],
          .name_repair = "unique_quiet"
        )
    }

  } else {
    # If a tibble was not requested, the output of emmeans::contrast() is
    # returned as is.
    out <- contrasts
  }

  out
}



# For each row in grid, return the index of its group's reference value.
get_ref_indices <- function(grid,
                            var,
                            ref_value,
                            grouping = NULL,
                            row_number_colname =
                              make_unique_name("rn", vec = colnames(grid))) {
  # If no grouping, return the index of the reference value, repeated once for
  # each row.
  if (is.null(grouping)) {
    return(rep_len(which(grid[[var]] == ref_value), nrow(grid)))
  }

  # Add a column of row numbers if it doesn't exist.
  if (!utils::hasName(grid, row_number_colname)) {
    grid[[row_number_colname]] <- seq_len(nrow(grid))
  }

  # Create a subset of grid containing only the rows containing the reference
  # value.
  ref_grid <- grid[grid[[var]] == ref_value, c(grouping, row_number_colname)]

  # Join the control grid into the main grid.
  grid <- dplyr::left_join(grid[grouping], ref_grid, by = grouping)

  # The last column will be the vector of indices of the reference values.
  grid[[ncol(grid)]]
}

