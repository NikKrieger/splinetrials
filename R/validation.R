

# Validate a flag argument (i.e., it must be TRUE or FALSE).
validate_flag <- function(x, arg = caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_bool(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}",
      "x" =
        if (is.logical(x)) {
          if (rlang::is_scalar_logical(x)) "It is {.code NA}." else
            "It is a logical vector of length {length(x)}."
        } else "It is an object of class {.cls {class(x)}}."
    ), call = call)
  }
  x
}


# Ensures x is a numeric vector that meets certain conditions.
validate_numeric <- function(x,
                             exact_length = NULL,
                             min_length = 0L,
                             max_length = Inf,
                             NA_ok = TRUE,
                             Inf_ok = TRUE,
                             duplicates_ok = TRUE,
                             unsorted_ok = TRUE,
                             arg = caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a numeric vector.",
      "x" = "You've supplied an object of class {.cls {class(x)}}."
    ),
    call = call)
  }

  if (is.null(exact_length)) {
    if (length(x) < min_length) {
      cli::cli_abort(c(
        "{.arg {arg}} must have at least {min_length} element{?s}.",
        "x" = "It has {length(x)}."
      ),
      call = call)
    }

    if (length(x) > max_length) {
      cli::cli_abort(c(
        "{.arg {arg}} must have at most {max_length} element{?s}.",
        "x" = "It has {length(x)}."
      ),
      call = call)
    }
  } else if (length(x) != exact_length) {
    cli::cli_abort(c(
      "{.arg {arg}} must have exactly {exact_length} element{?s}.",
      "x" = "It has {length(x)}."
    ),
    call = call)
  }

  if (!NA_ok && anyNA(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain missing values.",
      "x" = "NA/NaN values found at indices {which(is.na(x))}."
    ))
  }

  if (!Inf_ok && any(is.infinite(x))) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain Inf or -Inf.",
      "x" = "Infinite values detected at indices {which(is.infinite(x))}."
    ),
    call = call)
  }

  if (!duplicates_ok && anyDuplicated(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain duplicate values.",
      "x" = "Duplicate values found at indices: {which(duplicated(x))}."
    ),
    call = call)
  }

  if (!unsorted_ok && !identical(order(x), seq_along(x))) {
    cli::cli_abort(c(
      "{.arg {arg}} must be sorted in ascending order.",
      "x" = "The values are not in ascending order."
    ),
    call = call)
  }

  x
}




# Ensures x is a character vector meeting certain conditions.
validate_character <- function(x,
                               exact_length = NULL,
                               min_length = 0L,
                               max_length = Inf,
                               NA_ok = TRUE,
                               empty_strings_ok = TRUE,
                               duplicates_ok = TRUE,
                               unsorted_ok = TRUE,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a character vector.",
      "x" = "You've supplied an object of class {.cls {class(x)}}."
    ),
    call = call)
  }

  if (is.null(exact_length)) {
    if (length(x) < min_length) {
      cli::cli_abort(c(
        "{.arg {arg}} must have at least {min_length} element{?s}.",
        "x" = "It has {length(x)}."
      ),
      call = call)
    }

    if (length(x) > max_length) {
      cli::cli_abort(c(
        "{.arg {arg}} must have at most {max_length} element{?s}.",
        "x" = "It has {length(x)}."
      ),
      call = call)
    }
  } else if (length(x) != exact_length) {
    cli::cli_abort(c(
      "{.arg {arg}} must have exactly {exact_length} element{?s}.",
      "x" = "It has {length(x)}."
    ),
    call = call)
  }

  if (!NA_ok && anyNA(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain missing values.",
      "x" = "{.code NA_character_} found at indices: {which(is.na(x))}"
    ),
    call = call)
  }

  if (!empty_strings_ok && !all(nchar(x), na.rm = TRUE)) {
    cli::cli_abort(c(
      '{.arg {arg}} must not contain empty strings ({.code ""}).',
      "x" = "Empty strings found at indices: {which(nchar(x) == 0)}"
    ),
    call = call)
  }

  if (!duplicates_ok && anyDuplicated(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain duplicate values.",
      "x" = "Duplicate values found at indices: {which(duplicated(x) | duplicated(x, fromLast = TRUE))}"
    ),
    call = call)
  }

  if (!unsorted_ok && !identical(order(x), seq_along(x))) {
    cli::cli_abort(c(
      "{.arg {arg}} must be sorted in ascending order.",
      "x" = "The values are not in ascending order."
    ),
    call = call)
  }

  x
}





# Ensures that formula is a one-sided formula and that it does not specify a
# covariance structure
validate_covariates <- function(covariates, call = rlang::caller_env()) {
  if (!rlang::is_formula(covariates, lhs = FALSE)) {
    cli::cli_abort(c(
      "{.arg covariates} must be a {.cls formula} without a lefthand side.",
      "x" = if (rlang::is_formula(covariates)) "It has a lefthand side." else
        "You've supplied an object of class {.cls {class(covariates)}}."
    ),
    call = call)
  }

  cov_struct <- tryCatch(mmrm::as.cov_struct(covariates), error = identity)
  if (inherits(cov_struct, "cov_struct")) {
    cli::cli_abort(
      "Cannot supply a covariance structure to the {.arg covariates} argument. Control the covariance structure using the {.arg cov_structs}, {.arg time_observed_index}, {.arg subject}, and {.arg cov_struct_group} arguments.",
      call = call
    )
  }

  covariates
}





# Ensures cov_structs is a valid list of covariance structures as found in
# mmrm::cov_types(c("abbr", "habbr"))
validate_cov_structs <- function(cov_structs,
                                 time_observed_index,
                                 subject,
                                 cov_struct_group = NULL,
                                 data,
                                 call = rlang::caller_env()) {
  if (!is.character(cov_structs) && !is.list(cov_structs)) {
    cli::cli_abort(c(
      "{.arg cov_structs} must be a character vector or a list of {.cls cov_struct} objects.",
      "x" = "It is an object of class {.cls {class(cov_structs)[1L]}}."
    ),
    call = call)
  }

  if (!length(cov_structs)) {
    cli::cli_abort(c(
      "{.arg cov_structs} must have at least one element.",
      "x" = "It is empty."
    ),
    call = call)
  }

  if (is.character(cov_structs)) {
    cov_types <- mmrm::cov_types(c("abbr", "habbr"))

    if (anyNA(match(cov_structs, cov_types))) {
      cli::cli_abort(c(
        'If {.arg cov_structs} is a character vector, each element must be in the set {.code {cov_types}}.',
        "x" = "These elements are not: {setdiff(cov_structs, cov_types)}."
      ),
      call = call)
    }

    time_observed_index <-
      validate_user_var(
        {{time_observed_index}},
        make_factor_call = "as.ordered",
        data = data,
        call = call
      )

    # We turn these expressions into strings because that's what
    # mmrm::cov_struct() expects.
    time_observed_index <- rlang::expr_deparse(time_observed_index, width = Inf)
    subject <- validate_user_var({{subject}}, call = call)
    subject <- rlang::expr_deparse(subject, width = Inf)
    cov_struct_group <-
      validate_user_var(
        {{cov_struct_group}},
        null_ok = TRUE,
        make_factor_call = "as.factor",
        data = data,
        call = call
      )
    # If the user did not supply a cov_struct_group, use character() because
    # that's what mmrm::cov_struct() expects.
    cov_struct_group <- if (is.null(cov_struct_group)) quote(character()) else
        rlang::expr_deparse(cov_struct_group, width = Inf)

    # Create a list of calls to mmrm::cov_struct() whose only difference is the
    # type argument.
    cov_structs <-
      lapply(
        cov_structs,
        function(type, ...)
          rlang::call2("cov_struct", type = type, ..., .ns = "mmrm"),
        visits = time_observed_index,
        subject = subject,
        group = cov_struct_group
      )

  } else {

    # If the user supplied a list, it must be a list of cov_struct objects.
    is_cov_struct <- vapply(cov_structs, inherits, logical(1L), "cov_struct")
    if (!all(is_cov_struct)) {
      classes <- vapply(cov_structs, function(x) class(x)[1L], character(1L))
      non_cov_struct_indices <- as.character(which(!is_cov_struct))
      cli::cli_abort(c(
        "If {.arg cov_structs} is a list then all its elements must be {.cls cov_struct} objects.",
        "x" = "{cli::qty(classes)} Element{?s} {non_cov_struct_indices} {?is/are} of class{?es} {.cls {classes}}."
      ),
      call = call)
    }
  }

  if (anyDuplicated(cov_structs)) {
    cli::cli_abort(c(
      "{.arg cov_structs} must not contain duplicates.",
      "x" = "Elements {which(duplicated(cov_structs))} are duplicates."
    ),
    call = call)
  }

  cov_structs
}




# Checks the user-specified mmrm_args to make sure all elements are named and
# that they don't contain formula, data, or covariance.
# Adds in the formula and data arguments afterwards.
prepare_mmrm_args <- function(mmrm_args,
                              formula,
                              data_expr,
                              call = rlang::caller_env()) {

  mmrm_args <- validate_arg_list(mmrm_args, call = call)

  # The user is not allowed to supply arguments with these names.
  if (any(names(mmrm_args) %in% c("formula", "data", "covariance"))) {
    cli::cli_abort(c(
      "Cannot supply a {.arg formula}, {.arg data}, or {.arg covariance} argument to [mmrm::mmrm()].",
      "i" = "Control the covariance structure using the {.arg cov_structs}, {.arg time_observed_index}, {.arg subject}, and {.arg cov_struct_group} arguments."
    ),
    call = call)
  }

  mmrm_args[["formula"]] <- formula
  mmrm_args[["data"]] <- data_expr

  mmrm_args
}



# Ensures x is a single string. If data is supplied, x must also be a column
# name in data.
validate_single_string <- function(x,
                                   data = NULL,
                                   arg_x = caller_arg(x),
                                   arg_data = caller_arg(data),
                                   call = rlang::caller_env()) {
  if (!rlang::is_string(x, colnames(data))) {
    cli::cli_abort(c(
      if (is.null(data)) {
        "{.arg {arg_x}} must be a single nonmissing string."
      } else "{.arg {arg_x}} must be one of the column names of {.arg {arg_data}}.",
      "x" =
        if (rlang::is_string(x)) {
          "{.arg {arg_x}} not found among the columns of {.arg {arg_data}}, which are: {colnames(data)}."
        } else if (is.character(x)) {
          "It is a character vector of length {length(x)}."
        } else {
          "It is an object of class {.cls {class(x)}}."
        }
    ),
    call = call)
  }

  x
}



# Validates the user input for one of the model elements. Generally, the user
# can supply a string or a symbol. Some model elements can be expressions. Some
# elements are processed further to ensure they will evaluate to a factor or
# ordered factor.
validate_user_var <- function(x,
                              null_ok = FALSE,
                              require_symbol = FALSE,
                              make_factor_call =
                                c("no", "as.factor", "as.ordered"),
                              data = NULL,
                              arg = caller_arg(x, `{{` = TRUE),
                              call = rlang::caller_env()) {
  out <- rlang::quo_get_expr(rlang::enquo(x))
  make_factor_call <- match.arg(make_factor_call)

  if (is.null(out)) {
    if (null_ok) {
      return(NULL)
    }
    cli::cli_abort(c(
      "{.arg {arg}} must be an expression or a string.",
      "x" = "It is {.code NULL}."
    ), call = call)
  }

  if (rlang::is_string(out)) {
    out <- as.symbol(out)
  } else if (require_symbol && !rlang::is_symbol(out)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a string or a name identifying one variable.",
      "x" =
        if (rlang::is_call(out)) "It is a call." else
          if (rlang::is_syntactic_literal(out))
            "It is an object of type {typeof(out)}"
    ), call = call)
  }

  out <-
    switch(
      make_factor_call,
      no = out,
      make_factor_call(out, make_factor_call, data, call)
    )

  out
}




# Validate the numeric scheduled time variable and its corresponding label,
# ensuring a one-to-one relationship between the two.
validate_scheduled_time <- function(data,
                                    time_scheduled_continuous,
                                    time_scheduled_label,
                                    call = rlang::caller_env()) {
  distinct_data <- dplyr::distinct(data, .data[["time"]], .data[["time_num"]])

  duplicated_nums <-
    duplicated(distinct_data[["time"]]) |
    duplicated(distinct_data[["time"]], fromLast = TRUE)

  if (any(duplicated_nums)) {
    probs <- sort(unique(distinct_data[duplicated_nums, "time", drop = TRUE]))
    cli::cli_abort(c(
      "There must be a one-to-one relationship between the {.arg time_scheduled_continuous} variable, {.code {time_scheduled_continuous}}, and the {.arg time_scheduled_label} variable, {.code {time_scheduled_label}}.",
      "x" = "The {.code {time_scheduled_label}} {cli::qty(length(probs))} value{?s} {.code {probs}} {?is/are each} associated with multiple different {.code {time_scheduled_continuous}} values."
    ), call = call)
  }

  duplicated_labels <-
    duplicated(distinct_data[["time_num"]]) |
    duplicated(distinct_data[["time_num"]], fromLast = TRUE)

  if (any(duplicated_labels)) {
    probs <- sort(unique(distinct_data[duplicated_nums, "time_num", drop = TRUE]))
    cli::cli_abort(c(
      "There must be a one-to-one relationship between the {.arg time_scheduled_label} variable, {.code {time_scheduled_label}}, and the {.arg time_scheduled_continuous} variable, {.code {time_scheduled_continuous}}.",
      "x" = "The {.code {time_scheduled_continuous}} {cli::qty(length(probs))} value{?s} {.code {unique(distinct_data[['time_num']])}} {?is/are each} associated with multiple different {.code {time_scheduled_label}} values."
    ), call = call)
  }

  data
}




# Validates a reference value, ensuring it's actually present in the column.
validate_ref_value <- function(ref_value,
                               grid,
                               var_name,
                               var_arg = caller_arg(var_name),
                               call = rlang::caller_env()) {
  if (!any(grid[[var_name]] == ref_value)) {
    ref_value_name <-
      switch(
        var_name,
        arm = "control_group",
        subgroup = "subgroup_comparator"
      )
    cli::cli_abort(c(
      "The value supplied to the {.arg {ref_value_name}} argument must be present in the {.arg {var_arg}} variable, {.code {var_name}}.",
      "x" = "{.code {ref_value}} was not found in {.code {var_name}}."
    ),
    call = call)
  }
  ref_value
}




# Validates a list of arguments that will be supplied to a function, ensuring
# that all the arguments are uniquely named.
validate_arg_list <- function(arg_list,
                              arg = caller_arg(arg_list),
                              call = rlang::caller_env()) {
  if (!rlang::is_dictionaryish(arg_list)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be an empty vector or a named vector whose names are unique.",
      "x" =
        if (rlang::is_vector(arg_list)) {
          if (rlang::is_named2(arg_list)) {
            dup_names <- unique(names(arg_list)[duplicated(names(arg_list))])
            "Multiple arguments have the following name{?s}: {dup_names}."
          } else {
            empty_name_i <- seq_along(arg_list)[!rlang::have_name(arg_list)]
            "The following element{?s} {?has an/have} empty or missing name{?s}: {empty_name_i}."
          }
        } else {
          "It is an object of type {.cls {class(arg_list)}}."
        }
    ), call = call)
  }

  arg_list
}




# Validates a vector of values that will become an element of the "at" argument
# in emmeans::emmeans(). It must be a unique list of values and all of them must
# be among the factor levels of data[[var]].
validate_emmeans_spec <- function(spec,
                                  var,
                                  data,
                                  arg_spec = caller_arg(spec),
                                  arg_var = caller_arg(var),
                                  call = rlang::caller_env()) {
  spec <- as.character(spec)
  validate_character(
    spec,
    min_length = 1,
    NA_ok = FALSE,
    duplicates_ok = FALSE,
    call = call
  )
  if (!all(spec %in% levels(data[[var]]))) {
    cli::cli_abort(c(
      "All elements of {.arg {arg_spec}} must match the factor levels of {.arg {arg_var}}: {levels(data[[var]])}",
      "x" = "The following elements are also in {.arg {arg_spec}}: {spec[!spec %in% levels(data[[var]])]}"
    ), call = call)
  }
  spec
}
