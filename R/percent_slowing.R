

#' Calculates Percent Slowing from a Data Frame of Change-from-Baseline Data
#'
#' @description Accepts a data frame of change-from-baseline data (probably
#'   created with [change_from_baseline()]) and returns a table of percent
#'   slowing results.
#'
#' @inheritParams ncs_analysis_subgroup
#' @param change_from_bl_tbl (data frame)\cr a data frame of
#'   change-from-baseline data whose columns include `time_observed_continuous`,
#'   `arm`, (optionally) `subgroup`, `est`, and `se`.
#' @param time_observed_continuous,arm,subgroup,est,se (`string`)\cr strings
#'   identifying the columns in `change_from_bl_tbl` that contain the continuous
#'   time variable, the study arm, (optionally) the subgroup, the
#'   change-from-baseline estimate, and the change-from-baseline standard error.
#' @param control_group (`string`)\cr the value in the `arm` column of
#'   `change_from_bl_tbl` denoting the control group.
#'
#' @details For each study arm that is not the control group,
#'
#' \deqn{
#' \text{Let } \theta = \frac{\text{treatment estimate}}{\text{control estimate}}
#' }\deqn{
#' \text{Let } \alpha = 1 - \code{conf.level}
#' }\deqn{
#' \text{Let MOE} = 100 \times z_{1 - \alpha/2} \times \frac{\sqrt{\text{treatment SE}^2 + (\theta \times \text{control SE})^2}}{|\text{control estimate}|}
#' }
#'
#'   Therefore, the percent slowing estimates and their respective confidence
#'   intervals are calculated thus:
#'
#' \deqn{
#' \text{Percent slowing estimate} = (1 - \theta) \times 100
#' }\deqn{
#' \text{Percent slowing CI} = \text{Percent slowing estimate} \pm \text{MOE}
#' }
#'
#' @returns A data frame with a row for each combination of the unique values of
#'   `change_from_bl_tbl[[time_observed_continuous]]`,
#'   `change_from_bl_tbl[[arm]]` (except the value denoted in `control_group`),
#'   and `change_from_bl_tbl[[subgroup]]` (if `subgroup` is not `NULL`). It will
#'   contain the following columns:
#'
#'   1. \{column name will be the value of the `arm` argument\}: the study arm.
#'
#'   1. \{column name will be the value of the `time_observed_continuous`
#'   argument\}: the *observed* continuous time variable.
#'
#'   1. \{column name will be the value of the `subgroup` argument\}: the
#'   subgroup. Only present if `subgroup` is not `NULL`.
#'
#'   1. `percent_slowing_est`: the percent slowing estimate
#'
#'   1. `percent_slowing_lower`: the lower bound of the confidence interval for
#'   `percent_slowing_est`.
#'
#'   1. `percent_slowing_lower`: the upper bound of the confidence interval for
#'   `percent_slowing_est`.
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
#' marginal_means <-
#'   ncs_emmeans(
#'     fit = fit,
#'     observed_time = "time_cont",
#'     scheduled_time = "VISITN",
#'     arm = "ARMCD",
#'     subgroup = "SEX"
#'   )
#'
#' change_from_bl_tbl <-
#'   change_from_baseline(
#'     emmeans = marginal_means,
#'     time_observed_continuous = "time_cont",
#'     time_scheduled_baseline = 10,
#'     arm = "ARMCD",
#'     subgroup = "SEX",
#'     as_tibble = TRUE
#'   )
#'
#' percent_slowing_using_change_from_bl(
#'   change_from_bl_tbl = change_from_bl_tbl,
#'   time_observed_continuous = "time_cont",
#'   arm = "ARMCD",
#'   control_group = "PBO",
#'   subgroup = "SEX"
#' )
percent_slowing_using_change_from_bl <- function(change_from_bl_tbl,
                                                 time_observed_continuous,
                                                 arm,
                                                 control_group,
                                                 subgroup = NULL,
                                                 est = "estimate",
                                                 se = "SE",
                                                 conf.level = 0.95) {
  # Logical vector identifying the control arm rows in change_from_bl_tbl.
  trt_rows <- change_from_bl_tbl[[arm]] != control_group

  ctl_indices <-
    get_ref_indices(
      grid = change_from_bl_tbl,
      var = arm,
      ref_value = control_group,
      grouping = c(time_observed_continuous, subgroup)
    )

  ctl_indices <- ctl_indices[trt_rows]

  # Treatment rows' corresponding control estimates and SEs.
  ctl_est <- change_from_bl_tbl[ctl_indices, est, drop = TRUE]
  ctl_se <- change_from_bl_tbl[ctl_indices, se, drop = TRUE]

  # Remove control group rows from change_from_bl_tbl
  change_from_bl_tbl <- change_from_bl_tbl[trt_rows, , drop = FALSE]

  # The percent slowing calculations.
  theta <- change_from_bl_tbl[[est]] / ctl_est
  moe <-
    100 * stats::qnorm(conf.level / 2 + 0.5) *
    sqrt(change_from_bl_tbl[[se]] ^ 2 + (theta * ctl_se) ^ 2) / abs(ctl_est)

  change_from_bl_tbl <-
    change_from_bl_tbl[c(arm, time_observed_continuous, subgroup)]

  # Add the percent slowing estimates and confidence interval.
  pct_slow_est <-
    make_unique_name("percent_slowing_est", vec = colnames(change_from_bl_tbl))
  change_from_bl_tbl[[pct_slow_est]] <- 100 - 100 * theta

  pct_slow_lower <-
    make_unique_name(
      "percent_slowing_lower",
      vec = colnames(change_from_bl_tbl)
    )
  change_from_bl_tbl[[pct_slow_lower]] <-
    change_from_bl_tbl[[pct_slow_est]] - moe

  pct_slow_upper <-
    make_unique_name(
      "percent_slowing_upper",
      vec = colnames(change_from_bl_tbl)
    )
  change_from_bl_tbl[[pct_slow_upper]] <-
    change_from_bl_tbl[[pct_slow_est]] + moe

  change_from_bl_tbl
}
