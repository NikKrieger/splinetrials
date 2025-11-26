test_that("time_spline_basis() works", {
  expect_equal(
    time_spline_basis(
      test_data_subgroup$time_obs_cont, # c(test_data_subgroup$time_obs_cont, test_data_no_subgroup$time_obs_cont),
      df = 3
    ),
    test_basis,
    tolerance = 1e-3
  )
})



test_that("time_spline() works", {
  expect_warning(
    time_spline(
      test_data_subgroup$time_obs_cont,
      basis = test_basis,
      foo = "bar"
    ),
    "elements supplied to .?\\.\\.\\."
  )

  expect_warning(
    time_spline(
      test_data_subgroup$time_obs_cont,
      basis = test_basis,
      df = 0
    ),
    "df.? = 0 will be ignored"
  )

  expect_snapshot(time_spline(test_data_subgroup$time_obs_cont, df = 3))
})

