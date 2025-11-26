

#' Plot Outcome Variable by Timepoint and Study Arm
#'
#' [Plot][ggplot2::ggplot2-package] a continuous outcome for each combination of
#' scheduled visit and study arm.
#'
#' @param data (`data frame`)\cr The data frame that will be supplied to
#'   [ggplot2::ggplot()].
#' @param outcome_var (`numeric`)\cr The continuous outcome variable to supply
#'   to the `y` argument of [ggplot2::aes()]. Whatever is supplied will be
#'   [quoted and evaluated][rlang::embrace-operator] [in the context
#'   of][rlang::topic-data-mask] `data`.
#' @param scheduled_timepoint_var ([`ordered`])\cr The variable containing the
#'   scheduled timepoints to supply to the `x` argument of [ggplot2::aes()]..
#'   Whatever is supplied will be [quoted and
#'   evaluated][rlang::embrace-operator] [in the context
#'   of][rlang::topic-data-mask] `data`.
#' @param group_var (`numeric`)\cr The grouping variable (probably the study
#'   arm) to supply to the `fill` argument of [ggplot2::aes()].. Whatever is
#'   supplied will be [quoted and evaluated][rlang::embrace-operator] [in the
#'   context of][rlang::topic-data-mask] `data`.
#' @param ... Forwarded onto [`ggplot2::ggplot`]`(`[`ggplot2::aes`]`)`.
#' @param geom (`function`)\cr The `ggplot2` "geom" to use. Defaults to
#'   [ggplot2::geom_boxplot()].
#' @param geom_args (`list`)\cr A list of arguments to supply to `geom`.
#'   Defaults to `list(na.rm = TRUE)`.
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#' @export
plot_outcome_by_visit_and_group <- function(data,
                                            outcome_var,
                                            scheduled_timepoint_var,
                                            group_var,
                                            ...,
                                            geom = ggplot2::geom_boxplot,
                                            geom_args =
                                              list(na.rm = TRUE)) {
  rlang::check_installed("ggplot2")

  p <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = {{scheduled_timepoint_var}},
        y = {{outcome_var}},
        fill = {{group_var}},
        ...
      )
    ) +
    do.call(geom, geom_args)

  p
}





#' Plot Actual and Predicted Response Variable Means by Study Arm.
#'
#' This function accepts a data set, probably produced by [ncs_analysis()], and
#' it uses [ggplot2][ggplot2::ggplot2] to produce a [panel][ggplot2::facet_wrap]
#' of plots, one for each study `arm`. The `time` variable is along the x-axis,
#' and the response variable is along the y-axis. The actual means of the
#' response variable are [points][ggplot2::geom_point] plotted in one color, and
#' the modeled means are plotted in another color. Each point also has its
#' confidence interval [plotted][ggplot2::geom_errorbar].
#'
#' @param data (`data frame`)\cr a data frame, probably produced by
#'   [ncs_analysis()], containing the actual and predicted means. Each row
#'   should have a unique combination of `arm` and `time`.
#' @param arm (`string`)\cr the name of the study arm variable in `data`. There
#'   will be a [separate plot produced][ggplot2::facet_wrap] for each study arm.
#' @param time (`string`)\cr the name of the time or visit variable in `data`.
#'   These values correspond to the x-axis.
#' @param est,lower,upper (`string`)\cr the name of the variables in `data`
#'   containing the actual response variable's mean and confidence interval
#'   bounds. These values correspond to the y-axis.
#' @param model_est,model_lower,model_upper (`string`)\cr the name of the
#'   variables in `data` containing the predicted response variable's mean and
#'   confidence interval bounds. These values correspond to the y-axis.
#'
#' @returns An object returned by [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' # Create a usable data set out of mmrm::fev_data
#' fev_mod <- mmrm::fev_data
#' fev_mod$VISITN <- fev_mod$VISITN * 10
#' fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
#' fev_mod$obs_visit_index <- round(fev_mod$time_cont)
#'
#' # Analysis result data set
#' ncs_data_results <-
#'   ncs_analysis(
#'     data = fev_mod,
#'     response = FEV1,
#'     subject = USUBJID,
#'     arm = ARMCD,
#'     control_group = "PBO",
#'     time_observed_continuous = time_cont,
#'     df = 2,
#'     time_observed_index = obs_visit_index,
#'     time_scheduled_continuous = VISITN,
#'     time_scheduled_baseline = 10,
#'     time_scheduled_label = AVISIT,
#'     covariates = ~ FEV1_BL + RACE,
#'     cov_structs = c("ar1", "us")
#'   )
#'
#' ncs_plot_means(ncs_data_results)
ncs_plot_means <- function(data,
                           arm = "arm",
                           time = "time",
                           est = "est",
                           lower = "lower",
                           upper = "upper",
                           model_est = "response_est",
                           model_lower = "response_lower",
                           model_upper = "response_upper") {
  check_data <- data[c(arm, time, est, lower, upper)]
  check_response <-
    dplyr::select(
      data,
      !!arm,
      !!time,
      !!est := !!model_est,
      !!lower := !!model_lower,
      !!upper := !!model_upper
    )
  check <- dplyr::bind_rows(
    data = check_data,
    model = check_response,
    .id = "source"
  )
  check[[time]] <- factor(
    check[[time]],
    levels = data[data[[arm]] == data[1L, arm, drop = TRUE], time, drop = TRUE]
  )
  ggplot2::ggplot(check) +
    ggplot2::geom_point(
      ggplot2::aes(.data[[time]], .data[[est]], color = .data$source),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        .data[[time]],
        ymin = .data[[lower]],
        ymax = .data[[upper]],
        color = .data$source
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[[arm]])) +
    ggplot2::theme_gray(12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0))
}




#' Plot Actual and Predicted Response Variable Means by Study Arm and Subgroup.
#'
#' This function accepts a data set, probably produced by
#' [ncs_analysis_subgroup()], and it uses [ggplot2][ggplot2::ggplot2] to
#' produce a [grid][ggplot2::facet_grid] of plots, one for each combination of
#' study `arm` and `subgroup`. The `time` variable is along the x-axis, and the
#' response variable is along the y-axis. The actual means of the response
#' variable are [points][ggplot2::geom_point] plotted in one color, and the
#' modeled means are plotted in another color. Each point also has its
#' confidence interval [plotted][ggplot2::geom_errorbar].
#'
#' @param data (`data frame`)\cr a data frame, probably produced by
#'   [ncs_analysis()], containing the actual and predicted means. Each row
#'   should have a unique combination of `arm`, `time`, and `subgroup`.
#' @param arm (`string`)\cr the name of the study arm variable in `data`. There
#'   will be a [separate column of plots produced][ggplot2::facet_grid] for each
#'   study arm.
#' @param time (`string`)\cr the name of the time or visit variable in `data`.
#'   These values correspond to the x-axis.
#' @param subgroup (`string`)\cr the name of the subgroup variable in `data`.
#'   There will be a [separate row of plots produced][ggplot2::facet_grid] for
#'   each subgroup.
#' @param est,lower,upper (`string`)\cr the name of the variables in `data`
#'   containing the actual response variable's mean and confidence interval
#'   bounds. These values correspond to the y-axis.
#' @param model_est,model_lower,model_upper (`string`)\cr the name of the
#'   variables in `data` containing the predicted response variable's mean and
#'   confidence interval bounds. These values correspond to the y-axis.
#'
#' @returns An object returned by [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' # Create a usable data set out of mmrm::fev_data
#' fev_mod <- mmrm::fev_data
#' fev_mod$VISITN <- fev_mod$VISITN * 10
#' fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
#' fev_mod$obs_visit_index <- round(fev_mod$time_cont)
#'
#' # Analysis result data set
#' ncs_data_results_subgroup <-
#'   ncs_analysis_subgroup(
#'     data = fev_mod,
#'     response = FEV1,
#'     subject = USUBJID,
#'     arm = ARMCD,
#'     control_group = "PBO",
#'     subgroup = RACE,
#'     subgroup_comparator = "Asian",
#'     time_observed_continuous = time_cont,
#'     df = 2,
#'     time_observed_index = obs_visit_index,
#'     time_scheduled_continuous = VISITN,
#'     time_scheduled_baseline = 10,
#'     time_scheduled_label = AVISIT,
#'     covariates = ~ FEV1_BL + RACE,
#'     cov_structs = c("ar1", "us")
#'   )
#'
#' ncs_plot_means_subgroup(ncs_data_results_subgroup$between)
ncs_plot_means_subgroup <- function(data,
                                    arm = "arm",
                                    time = "time",
                                    subgroup = "subgroup",
                                    est = "est",
                                    lower = "lower",
                                    upper = "upper",
                                    model_est = "response_est",
                                    model_lower = "response_lower",
                                    model_upper = "response_upper") {
  check_data <- data[c(arm, time, subgroup, est, lower, upper)]
  check_response <-
    dplyr::select(
      data,
      !!arm,
      !!time,
      !!subgroup,
      !!est := !!model_est,
      !!lower := !!model_lower,
      !!upper := !!model_upper
    )
  check <- dplyr::bind_rows(
    data = check_data,
    model = check_response,
    .id = "source"
  )
  check[[time]] <- factor(
    check[[time]],
    levels =
      data[
        data[[arm]] == data[1L, arm, drop = TRUE] &
          data[[subgroup]] == data[1L, subgroup, drop = TRUE],
        time,
        drop = TRUE
      ]
  )
  ggplot2::ggplot(check) +
    ggplot2::geom_point(
      ggplot2::aes(.data[[time]], .data[[est]], color = .data$source),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        .data[[time]],
        ymin = .data[[lower]],
        ymax = .data[[upper]],
        color = .data$source
      ),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[[subgroup]]),
      cols = ggplot2::vars(.data[[arm]])
    ) +
    ggplot2::theme_gray(12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0))
}





#' Categorize Observed Timepoints According to Scheduled Timepoints
#'
#' Create an [ordered] factor from a vector of observed values, associating each
#' observed value with the [level][levels] corresponding to a vector of
#' expected/`scheduled` values.
#'
#' @param observed A numeric vector of values.
#' @param scheduled A numeric vector of unique, [finite] values. Length must be
#'   at least 2. The default is to take the [unique], [finite] values of
#'   `observed`.
#' @param breaks A numeric vector of unique values. `-Inf` and `Inf` are valid.
#'   Passed to [cut()]. The default is to take the midpoints of `scheduled` and
#'   to put them in between `c(-Inf, [Inf])`.
#' @param labels A vector of labels for the resulting [`ordered`] factor. Passed
#'   to [cut()]. Must have [length()] equal to `scheduled`. Defaults to
#'   `"Baseline"` as the first level's label and `"Visit#"` for all subsequent
#'   [levels], where `#` is the numeric index of the timepoint minus 1.
#' @param ... Additional arguments passed to [cut()].
#' @returns And [`ordered] factor` with the same length as `observed`.
#' @export
#'
#' @examples
#' observed_timepoints <- c(0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
#' scheduled_timepoints <- c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 50, 75)
#' bin_timepoints(
#'   observed_timepoints,
#'   scheduled = scheduled_timepoints
#' )
#'
#' bin_timepoints(
#'   observed_timepoints,
#'   scheduled = scheduled_timepoints,
#'   breaks = c(-Inf, 0.1, 1.5, 2.5, 3.5, 4.4, 7, 11, 15.1, 21, 31, 58, 80)
#' )
#'
#' bin_timepoints(
#'   observed_timepoints,
#'   scheduled = scheduled_timepoints,
#'   labels = month.name
#' )
#'
#' bin_timepoints(
#'   observed_timepoints,
#'   scheduled = scheduled_timepoints,
#'   labels = make_visit_labels(scheduled_timepoints, visit = "Week")
#' )
#'
#' bin_timepoints(observed_timepoints)
bin_timepoints <- function (observed,
                            scheduled = unique(observed[!is.na(observed)]),
                            breaks = c(-Inf, midpoints(scheduled), Inf),
                            labels =
                              make_visit_labels(seq_along(scheduled) - 1),
                            ...) {
  validate_numeric(observed, Inf_ok = FALSE)
  validate_numeric(
    scheduled,
    min_length = 2L,
    NA_ok = FALSE,
    Inf_ok = FALSE,
    duplicates_ok = FALSE
  )

  validate_numeric(breaks, NA_ok = FALSE, duplicates_ok = FALSE)
  if (length(breaks) != length(scheduled) + 1L) {
    cli::cli_abort(c(
      "Length of {.arg breaks} must be equal to `length(scheduled) + 1 = {length(scheduled) + 1}`.",
      "x" = "It is {length(breaks)}."
    ))
  }

  if (length(labels) != length(scheduled)) {
    cli::cli_abort(c(
      "Length of {.arg labels} must be equal to `length(scheduled) = {length(scheduled)}`.",
      "x" = "It is {length(labels)}."
    ))
  }

  cut(
    observed,
    breaks = breaks,
    labels = labels,
    ordered_result = TRUE,
    ...
  )
}


#' Midpoints of a Numeric Vector
#'
#' Returns the midpoints between the elements of a vector in the order the
#' elements appear.
#'
#' This function does not sort.
#'
#' @param x A numeric vector with at least 2 elements.
#'
#' @returns A numeric vector of [length] `length(x) - 1`.
#' @export
#'
#' @examples
#' midpoints(c(0, 1, 10, 4))
midpoints <- function(x) {
  validate_numeric(x, min_length = 2L)
  out <- (x[-1L] + x[-length(x)]) / 2
  out
}






#' Make Visit Labels Based on a Numeric Vector
#'
#' Create a character vector of values to be used as labels for a [factor].
#'
#' Places `visit` as a prefix before the values of `t`. If `pad` is not `NULL`,
#' the values of `t` are first `format`ted so that their places are aligned, and
#' they are left-padded with zeros.
#'
#' If `baseline` is not `NULL` it is used as the first label regardless of the
#' value of `t[1]`.
#'
#' Uses [`make.unique`]`(sep = "_")` in case any elements are identical after
#' [format]ting.
#'
#' @param t A non-empty numeric vector of unique, [finite] elements in ascending
#'   order.
#' @param visit A single character string specifying the prefix to add to `t`.
#' @param baseline A single character string to use for the first timepoint's
#'   label. Alternatively, set to `NULL` so that all timepoints will have the
#'   prefix specified by `visit`.
#' @param pad The character to use to pad between `visit` and `t` so that
#'   the places of `t` are aligned. Alternatively, set to `NULL` so that `t` is
#'   automatically converted to `character` without special formatting. This can
#'   result in numbers in labels not being aligned or not being in
#'   "alphabetical" [order].
#'
#' @returns A [character] vector of length [`length`]`(t)`.
#' @export
#'
#' @examples
#' make_visit_labels(c(0, 5, 13, 101))
#'
#' make_visit_labels(c(0, 5.23453, 13, 101.4))
#'
#' make_visit_labels(c(0, 5.23453, 13, 101.4), baseline = NULL, pad = " ")
#'
#' make_visit_labels(c(0, 5.23453, 13, 101.4), visit = "Week", pad = NULL)
make_visit_labels <- function(t,
                              visit = "VIS",
                              baseline = "BASELINE",
                              pad = "0") {
  validate_numeric(
    t,
    min_length = 1L,
    NA_ok = FALSE,
    Inf_ok = FALSE,
    duplicates_ok = FALSE,
    unsorted_ok = FALSE
  )

  if (!is.null(baseline)) {
    t <- t[-1L]
  }

  if (!is.null(pad)) {
    nchar_integer_part <- nchar(trunc(t))
    decimal_width <- nchar(t) - nchar_integer_part
    width <- max(decimal_width) + max(nchar_integer_part)
    nsmall <- max(decimal_width - 1, 0)
    t <- format(t, width = width, nsmall = nsmall)
    t <- gsub(" ", pad, t)
    t <- make.unique(t, sep = "_")
  }

  c(baseline, paste0(visit, t))
}
