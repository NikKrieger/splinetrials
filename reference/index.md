# Package index

## Full NCS Analyses

Execute a full NCS analysis on a data set.

- [`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)
  [`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)
  : Run a Natural Cubic Spline (NCS) Analysis.

- [`splinetrials_analysis-class`](https://nikkrieger.github.io/splinetrials/reference/splinetrials_analysis-class.md)
  :

  `splinetrials_analysis` object

- [`splinetrials_subgroup_analysis-class`](https://nikkrieger.github.io/splinetrials/reference/splinetrials_subgroup_analysis-class.md)
  :

  `splinetrials_subgroup_analysis` object

## Individual NCS Analysis Components

Execute one of the components of the full NCS analyses

- [`ncs_mmrm_fit()`](https://nikkrieger.github.io/splinetrials/reference/ncs_mmrm_fit.md)
  : Create a Mixed Model with Repeated Measures Using Natural Cubic
  Splines.
- [`ncs_emmeans()`](https://nikkrieger.github.io/splinetrials/reference/ncs_emmeans.md)
  : Estimate Marginal Means for a Natural Cubic Splines Analysis
- [`change_from_baseline()`](https://nikkrieger.github.io/splinetrials/reference/treatment_effect.md)
  [`treatment_effect()`](https://nikkrieger.github.io/splinetrials/reference/treatment_effect.md)
  : Calculate the Change from Baseline or Treatment Effects from
  Estimated Marginal Means
- [`percent_slowing_using_change_from_bl()`](https://nikkrieger.github.io/splinetrials/reference/percent_slowing_using_change_from_bl.md)
  : Calculates Percent Slowing from a Data Frame of Change-from-Baseline
  Data

## Plotting

Plot the actual and modeled NCS data.

- [`ncs_plot_means()`](https://nikkrieger.github.io/splinetrials/reference/ncs_plot_means.md)
  : Plot Actual and Predicted Response Variable Means by Study Arm.
- [`ncs_plot_means_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_plot_means_subgroup.md)
  : Plot Actual and Predicted Response Variable Means by Study Arm and
  Subgroup.
- [`plot_outcome_by_visit_and_group()`](https://nikkrieger.github.io/splinetrials/reference/plot_outcome_by_visit_and_group.md)
  : Plot Outcome Variable by Timepoint and Study Arm

## Data prep

Prepare a data set for natural cubic splines analysis.

- [`bin_timepoints()`](https://nikkrieger.github.io/splinetrials/reference/bin_timepoints.md)
  : Categorize Observed Timepoints According to Scheduled Timepoints
- [`make_visit_labels()`](https://nikkrieger.github.io/splinetrials/reference/make_visit_labels.md)
  : Make Visit Labels Based on a Numeric Vector
- [`midpoints()`](https://nikkrieger.github.io/splinetrials/reference/midpoints.md)
  : Midpoints of a Numeric Vector

## Time Splines

Helpers for applying natural cubic splines to a numeric vector.

- [`time_spline()`](https://nikkrieger.github.io/splinetrials/reference/time_spline.md)
  : Create Natural Cubic Spline Approximations for Continuous Time
- [`time_spline_basis()`](https://nikkrieger.github.io/splinetrials/reference/time_spline_basis.md)
  : Natural Cubic Spline Basis Matrix for Continuous Time.
