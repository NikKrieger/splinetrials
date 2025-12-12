# `splinetrials_analysis` object

[`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)
returns an object of class `splinetrials_analysis`: a 32-column
[`tibble`](https://dplyr.tidyverse.org/reference/reexports.html) with
one row per unique combination of `data[[arm]]` and
`data[[time_scheduled_label]]` (see the arguments of
[`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)).

## Columns

1.  `arm`: values of `data[[arm]]`.

2.  `time`: values of `data[[time_scheduled_label]]`.

3.  `n`: number of times the combination appears in data.

4.  `est`: [mean](https://rdrr.io/r/base/mean.html) of
    `data[[response]]`.

5.  `sd`: [standard deviation](https://rdrr.io/r/stats/sd.html) of
    `data[[response]]`.

6.  `se`: standard error of `data[[response]]` (i.e., `sd / sqrt(n)`).

7.  `lower`: lower bound of confidence interval.

8.  `upper`: upper bound of confidence interval.

9.  `response_est`: estimated marginal mean.

10. `response_se`: standard error of `response_est`.

11. `response_df`: degrees of freedom used for calculating the
    confidence interval for `response_est`.

12. `response_lower`: lower bound of confidence interval for
    `response_est`.

13. `response_upper`: upper bound of confidence interval for
    `response_est`.

14. `change_est`: estimated change from baseline.

15. `change_se`: standard error of `change_est`.

16. `change_df`: degrees of freedom used for calculating the confidence
    interval for and testing the significance of `change_est`.

17. `change_lower`: lower bound of confidence interval for `change_est`.

18. `change_upper`: upper bound of confidence interval for `change_est`.

19. `change_test_statistic`: test statistic measuring the significance
    of `change_est`.

20. `change_p_value`: p-value for the significance of `change_est`.

21. `diff_est`: treatment effect.

22. `diff_se`: standard error of `diff_est`.

23. `diff_df`: degrees of freedom used for calculating the confidence
    interval for and testing the significance of `diff_est`.

24. `diff_lower`: lower bound of confidence interval for `diff_est`.

25. `diff_upper`: upper bound of confidence interval for `diff_est`.

26. `diff_test_statistic`: test statistic measuring the significance of
    `diff_est`.

27. `diff_p_value`: p-value for the significance of `diff_est`.

28. `percent_slowing_est`: estimated percent slowing.

29. `percent_slowing_lower`: lower bound of confidence interval for
    `percent_slowing_est`.

30. `percent_slowing_upper`: upper bound of confidence interval for
    `percent_slowing_est`.

31. `correlation`: the covariance structure of the analysis model. This
    is the same value repeated for each row.

32. `optimizer`: invariably `mmrm+tmb` to indicate that
    [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
    (which uses the `TMB` package) was used to fit the model.

## Optional `analysis_model` attribute

If
[`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)
had `return_models = TRUE`, then the analysis model, an `mmrm` object,
will be included as the `analysis_model`
[attr](https://rdrr.io/r/base/attr.html)ibute.

## See also

The function
[`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
which produces objects of this class.
