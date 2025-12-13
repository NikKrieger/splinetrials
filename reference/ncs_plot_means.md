# Plot Actual and Predicted Response Variable Means by Study Arm.

This function accepts a data set, probably produced by
[`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
and it uses
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
to produce a
[panel](https://ggplot2.tidyverse.org/reference/facet_wrap.html) of
plots, one for each study `arm`. The `time` variable is along the
x-axis, and the response variable is along the y-axis. The actual means
of the response variable are
[points](https://ggplot2.tidyverse.org/reference/geom_point.html)
plotted in one color, and the modeled means are plotted in another
color. Each point also has its confidence interval
[plotted](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

## Usage

``` r
ncs_plot_means(
  data,
  arm = "arm",
  time = "time",
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
  unique combination of `arm` and `time`.

- arm:

  (`string`)  
  the name of the study arm variable in `data`. There will be a
  [separate plot
  produced](https://ggplot2.tidyverse.org/reference/facet_wrap.html) for
  each study arm.

- time:

  (`string`)  
  the name of the time or visit variable in `data`. These values
  correspond to the x-axis.

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
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

# Analysis result data set
ncs_data_results <-
  ncs_analysis(
    data = fev_mod,
    response = FEV1,
    subject = USUBJID,
    arm = ARMCD,
    control_group = "PBO",
    time_observed_continuous = time_cont,
    df = 2,
    time_observed_index = obs_visit_index,
    time_scheduled_continuous = VISITN,
    time_scheduled_baseline = 10,
    time_scheduled_label = AVISIT,
    covariates = ~ FEV1_BL + RACE,
    cov_structs = c("ar1", "us")
  )
#> In as.ordered(obs_visit_index) there are dropped visits: 13, 43.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.

ncs_plot_means(ncs_data_results)
```
