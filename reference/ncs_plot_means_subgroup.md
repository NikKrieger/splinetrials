# Plot Actual and Predicted Response Variable Means by Study Arm and Subgroup.

This function accepts a data set, probably produced by
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
and it uses
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
to produce a
[grid](https://ggplot2.tidyverse.org/reference/facet_grid.html) of
plots, one for each combination of study `arm` and `subgroup`. The
`time` variable is along the x-axis, and the response variable is along
the y-axis. The actual means of the response variable are
[points](https://ggplot2.tidyverse.org/reference/geom_point.html)
plotted in one color, and the modeled means are plotted in another
color. Each point also has its confidence interval
[plotted](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

## Usage

``` r
ncs_plot_means_subgroup(
  data,
  arm = "arm",
  time = "time",
  subgroup = "subgroup",
  est = "est",
  lower = "lower",
  upper = "upper",
  model_est = "response_est",
  model_lower = "response_lower",
  model_upper = "response_upper"
)
```

## Arguments

- data:

  (`data frame`)  
  a data frame, probably produced by
  [`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
  containing the actual and predicted means. Each row should have a
  unique combination of `arm`, `time`, and `subgroup`.

- arm:

  (`string`)  
  the name of the study arm variable in `data`. There will be a
  [separate column of plots
  produced](https://ggplot2.tidyverse.org/reference/facet_grid.html) for
  each study arm.

- time:

  (`string`)  
  the name of the time or visit variable in `data`. These values
  correspond to the x-axis.

- subgroup:

  (`string`)  
  the name of the subgroup variable in `data`. There will be a [separate
  row of plots
  produced](https://ggplot2.tidyverse.org/reference/facet_grid.html) for
  each subgroup.

- est, lower, upper:

  (`string`)  
  the name of the variables in `data` containing the actual response
  variable's mean and confidence interval bounds. These values
  correspond to the y-axis.

- model_est, model_lower, model_upper:

  (`string`)  
  the name of the variables in `data` containing the predicted response
  variable's mean and confidence interval bounds. These values
  correspond to the y-axis.

## Value

An object returned by
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
if (FALSE) { # interactive()
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

# Analysis result data set
ncs_data_results_subgroup <-
  ncs_analysis_subgroup(
    data = fev_mod,
    response = FEV1,
    subject = USUBJID,
    arm = ARMCD,
    control_group = "PBO",
    subgroup = RACE,
    subgroup_comparator = "Asian",
    time_observed_continuous = time_cont,
    df = 2,
    time_observed_index = obs_visit_index,
    time_scheduled_continuous = VISITN,
    time_scheduled_baseline = 10,
    time_scheduled_label = AVISIT,
    covariates = ~ FEV1_BL + RACE,
    cov_structs = c("ar1", "us")
  )

ncs_plot_means_subgroup(ncs_data_results_subgroup$between)
}
```
