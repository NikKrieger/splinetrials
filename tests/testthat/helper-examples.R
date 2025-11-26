# #######################################################################
# Data
# #######################################################################

# These unit tests only use simulated data.

# No subgroup
set.seed(1)
test_data_no_subgroup <-
  dplyr::mutate(
    dplyr::cross_join(
      data.frame(
        patient = do.call(paste0, expand.grid(LETTERS, letters))[1:50],
        arm =
          sample(c("control", "active1", "active2"), 50, replace = TRUE),
        categorical1 = sample(paste0("level", 1:3), 50, replace = TRUE),
        categorical2 = sample(c("level1", "level2"), 50, replace = TRUE),
        continuous1 = rnorm(50),
        continuous2 = rnorm(50)
      ),
      data.frame(
        time_observed_index = seq_len(8),
        time_scheduled_index = seq_len(8),
        time_scheduled_label =
          c("Baseline", paste("visit", c(3, 4, 5, 7, 8, 9, 12))),
        time_sch_cont = seq(from = 0, length.out = 8) / 4
      )
    ),
    time_obs_cont =
      time_sch_cont + runif(dplyr::n(), min = -0.1, max = 0.1),
    response =
      rnorm(
        dplyr::n(),
        10 - (1 * (arm == "active1") + 2 * (arm == "active2") +
                3 * (arm == "control")) * sqrt((time_observed_index / 4) -
                                                 min(time_observed_index / 4))
      )
  )

# With subgroup
set.seed(1)
test_data_subgroup <-
  dplyr::mutate(
    dplyr::cross_join(
      data.frame(
        patient = do.call(paste0, expand.grid(LETTERS, letters))[1:30],
        arm = rep(c("control", "active1", "active2"), each = 10),
        subgroup = rep(c("subgroup1", "subgroup2", "subgroup3"), times = 10),
        categorical1 = sample(paste0("level", 1:3), 30, replace = TRUE),
        categorical2 = sample(c("level1", "level2"), 30, replace = TRUE),
        continuous1 = rnorm(30),
        continuous2 = rnorm(30)
      ),
      data.frame(
        time_observed_index = seq_len(8),
        time_scheduled_index = seq_len(8),
        time_scheduled_label =
          c("Baseline", paste("visit", c(3, 4, 5, 7, 8, 9, 12))),
        time_sch_cont = as.numeric(seq(from = 0, length.out = 8) / 4)
      )
    ),
    time_obs_cont =
      time_sch_cont + runif(dplyr::n(), min = -0.1, max = 0.1),
    response = rnorm(
      dplyr::n(),
      mean =
        10 - (1 * (arm == "active1") + 2 * (arm == "active2") +
                3 * (arm == "control")) * sqrt((time_observed_index / 4) -
                                                 min(time_observed_index / 4))
    )
  )


# Data that will lead to a convergence failure
test_data_convergence_failure <-
  data.frame(
    FEV1 = c(1, 2, 3, 4, 5),
    FEV1_BL = c(1, 2, 1, 2, 1),
    AVISIT = factor(c("V1", "V1", "V2", "V3", "V4")),
    SEX = factor(c("M", "F", "F", "M", "F")),
    USUBJID = c("A", "B", "A", "C", "A")
  )




# Spline basises
test_basis <-
  splines::ns(
    test_data_subgroup$time_obs_cont, # c(test_data_subgroup$time_obs_cont, test_data_no_subgroup$time_obs_cont),
    df = 3,
    Boundary.knots =
      c(0,  # max(c(test_data_subgroup$time_obs_cont, test_data_no_subgroup$time_obs_cont),
        max(test_data_subgroup$time_obs_cont, na.rm = TRUE))
  )

test_basis_no_subgroup <-
  splines::ns(
    test_data_no_subgroup$time_obs_cont, # c(test_data_subgroup$time_obs_cont, test_data_no_subgroup$time_obs_cont),
    df = 3,
    Boundary.knots =
      c(0,  # max(c(test_data_subgroup$time_obs_cont, test_data_no_subgroup$time_obs_cont),
        max(test_data_no_subgroup$time_obs_cont, na.rm = TRUE))
  )



# ##############################################################################
# No subgroup analysis
# ##############################################################################

test_ncs_analysis <-
  ncs_analysis(
    data = test_data_no_subgroup,
    response = "response",
    subject = "patient",
    arm = arm,
    control_group = "control",
    time_observed_continuous =
      "time_obs_cont",
    df = 3,
    time_observed_index = "time_observed_index",
    time_scheduled_continuous =
      "time_sch_cont",
    time_scheduled_baseline = 0,
    time_scheduled_label =
      "time_scheduled_label",
    covariates = ~ categorical2 + continuous1,
    cov_structs = "us",
    mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb"),
    return_models = TRUE
  )

test_env_no_subgroup <-
  environment(
    attr(test_ncs_analysis, "splinetrials_analysis_model")$formula_parts$formula
  )

# ##############################################################################
# subgroup analysis
# ##############################################################################

test_ncs_analysis_subgroup <-
  ncs_analysis_subgroup(
    data = test_data_subgroup,
    response = "response",
    subject = "patient",
    arm = arm,
    control_group = "control",
    subgroup = "subgroup",
    subgroup_comparator = "subgroup1",
    time_observed_continuous =
      "time_obs_cont",
    df = 3,
    time_observed_index = "time_observed_index",
    time_scheduled_continuous =
      "time_sch_cont",
    time_scheduled_baseline = 0,
    time_scheduled_label =
      "time_scheduled_label",
    covariates = ~ categorical2 + continuous1,
    cov_structs = "us",
    mmrm_args = list(method = "Satterthwaite", optimizer = "nlminb"),
    subgroup_interaction_test = TRUE,
    return_models = TRUE
  )

test_env_subgroup <-
  environment(
    test_ncs_analysis_subgroup$analysis_model$formula_parts$formula
  )

# #######################################################################
# Model fitting
# #######################################################################

# No subgroup
test_fit_no_subgroup <-
  evalq(
    mmrm::mmrm(
      formula =
        response ~
        spline_fn(time_obs_cont)[, 1] + spline_fn(time_obs_cont)[, 2] + spline_fn(time_obs_cont)[, 3] +
        categorical2 +
        continuous1 +
        spline_fn(time_obs_cont)[, 1]:arm +
        spline_fn(time_obs_cont)[, 2]:arm +
        spline_fn(time_obs_cont)[, 3]:arm,
      data =
        dplyr::mutate(
          test_data_no_subgroup,
          arm = factor(arm, c("control", "active1", "active2"))
        ),
      covariance =
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(time_observed_index)", # "as.ordered(paste0(sch_time_env_no_subgroup, foo))",
          subject = "patient",
          group = character()
        )
    ),
    envir = test_env_no_subgroup
  )

# Subgroup: analysis model
test_fit_subgroup <-
  evalq(
    mmrm::mmrm(
      formula =
        response ~
        spline_fn(time_obs_cont)[, 1] +
        spline_fn(time_obs_cont)[, 2] +
        spline_fn(time_obs_cont)[, 3] +
        subgroup +
        categorical2 +
        continuous1 +
        spline_fn(time_obs_cont)[, 1]:subgroup +
        spline_fn(time_obs_cont)[, 2]:subgroup +
        spline_fn(time_obs_cont)[, 3]:subgroup +
        spline_fn(time_obs_cont)[, 1]:arm +
        spline_fn(time_obs_cont)[, 2]:arm +
        spline_fn(time_obs_cont)[, 3]:arm +
        spline_fn(time_obs_cont)[, 1]:subgroup:arm +
        spline_fn(time_obs_cont)[, 2]:subgroup:arm +
        spline_fn(time_obs_cont)[, 3]:subgroup:arm,
      data =
        dplyr::mutate(
          test_data_subgroup,
          arm = factor(arm, c("control", "active1", "active2")),
          subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3"))
        ),
      covariance =
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(time_observed_index)", # "as.ordered(paste0(sch_time_env, foo))",
          subject = "patient",
          group = character()
        ),
      method = "Satterthwaite",
      optimizer = "nlminb"
    ),
    envir = test_env_subgroup
  )

# Subgroup: full model
test_fit_subgroup_full <-
  evalq(
    mmrm::mmrm(
      response ~
        spline_fn(time_obs_cont)[, 1] +
        spline_fn(time_obs_cont)[, 2] +
        spline_fn(time_obs_cont)[, 3] +
        subgroup +
        categorical2 +
        continuous1 +
        spline_fn(time_obs_cont)[, 1]:subgroup +
        spline_fn(time_obs_cont)[, 2]:subgroup +
        spline_fn(time_obs_cont)[, 3]:subgroup +
        spline_fn(time_obs_cont)[, 1]:arm +
        spline_fn(time_obs_cont)[, 2]:arm +
        spline_fn(time_obs_cont)[, 3]:arm +
        spline_fn(time_obs_cont)[, 1]:subgroup:arm +
        spline_fn(time_obs_cont)[, 2]:subgroup:arm +
        spline_fn(time_obs_cont)[, 3]:subgroup:arm,
      data =
        dplyr::mutate(
          test_data_subgroup,
          arm = factor(arm, c("control", "active1", "active2")),
          subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3"))
        ),
      covariance =
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(time_observed_index)", # "as.ordered(paste0(sch_time_env, foo))",
          subject = "patient",
          group = character()
        ),
      method = "Satterthwaite",
      optimizer = "nlminb",
      reml = FALSE
    ),
    envir = test_env_subgroup
  )

# Subgroup: reduced model
test_fit_subgroup_reduced <-
  evalq(
    mmrm::mmrm(
      formula =
        response ~
        spline_fn(time_obs_cont)[, 1] +
        spline_fn(time_obs_cont)[, 2] +
        spline_fn(time_obs_cont)[, 3] +
        subgroup +
        categorical2 +
        continuous1 +
        spline_fn(time_obs_cont)[, 1]:subgroup +
        spline_fn(time_obs_cont)[, 2]:subgroup +
        spline_fn(time_obs_cont)[, 3]:subgroup +
        spline_fn(time_obs_cont)[, 1]:arm +
        spline_fn(time_obs_cont)[, 2]:arm +
        spline_fn(time_obs_cont)[, 3]:arm,
      data =
        dplyr::mutate(
          test_data_subgroup,
          arm = factor(arm, c("control", "active1", "active2")),
          subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3"))
        ),
      covariance =
        mmrm::cov_struct(
          type = "us",
          visits = "as.ordered(time_observed_index)", # "as.ordered(paste0(sch_time_env, foo))",
          subject = "patient",
          group = character()
        ),
      method = "Satterthwaite",
      optimizer = "nlminb",
      reml = FALSE
    ),
    envir = test_env_subgroup,
  )


# ##############################################################################
# emmeans
# ##############################################################################

# No subgroup
unique_times_no_subgroup <-
  sort(unique(test_fit_no_subgroup[["data"]][["time_sch_cont"]]))
test_fit_for_emmeans_no_subgroup <- test_fit_no_subgroup
test_fit_for_emmeans_no_subgroup[["data"]] <-
  dplyr::mutate(
    test_fit_for_emmeans_no_subgroup[["data"]],
    arm = factor(arm, c("control", "active1", "active2")),
    # sch_time_env = time_sch_cont,
    .before = weights
  )
emmeans_results_no_subgroup <-
  emmeans::emmeans(
    object = test_fit_for_emmeans_no_subgroup,
    data = test_fit_for_emmeans_no_subgroup[["data"]],
    specs = c("arm", "time_obs_cont"),
    at = list(arm = c("control", "active1", "active2"),
              time_obs_cont = unique_times_no_subgroup),
    nuisance = c("categorical2", "continuous1"),
    nesting = NULL,
    params =
      c( # "foo",
        '"control"', '"active1"', '"active2"')
  )
tryCatch(emmeans_results_no_subgroup@model.info$nesting <- NULL, error = identity)
tryCatch(emmeans_results_no_subgroup@misc$display <- NULL, error = identity)

# With subgroup
unique_times_subgroup <-
  sort(unique(test_fit_subgroup[["data"]][["time_sch_cont"]]))
test_fit_for_emmeans <- test_fit_subgroup
test_fit_for_emmeans[["data"]] <-
  dplyr::mutate(
    test_fit_for_emmeans[["data"]],
    arm = factor(arm, c("control", "active1", "active2")),
    subgroup = factor(subgroup, c("subgroup1", "subgroup2", "subgroup3")),
    # sch_time_env = time_sch_cont,
    .before = weights
  )
emmeans_results_subgroup <-
  emmeans::emmeans(
    object = test_fit_for_emmeans,
    data = test_fit_for_emmeans[["data"]],
    specs = c("arm", "time_obs_cont", "subgroup"),
    at = list(arm = c("control", "active1", "active2"),
              time_obs_cont = unique_times_subgroup,
              subgroup = c("subgroup1", "subgroup2", "subgroup3")),
    nuisance = c("categorical2", "continuous1"),
    nesting = NULL,
    params =
      c( # "foo",
        '"subgroup1"', '"subgroup2"', '"subgroup3"', '"control"', '"active1"', '"active2"')
  )
tryCatch(emmeans_results_subgroup@model.info$nesting <- NULL, error = identity)
tryCatch(emmeans_results_subgroup@misc$display <- NULL, error = identity)
