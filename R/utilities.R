
# Prevents R CHECK from whining about these tidy evaluation tools.
utils::globalVariables(c(".data", ":="))

# Modifed version of rlang::caller_arg() that can handle arguments
# wrapped in `{{` e.g. {{x}}
caller_arg <- function(arg, `{{` = FALSE) {
  # Evaluate grab the expression that was supplied in the calling environment.
  arg <- substitute(arg)
  expr <- eval.parent(substitute(substitute(foo), list(foo = arg)))

  # If `{{` = TRUE and the argument was wrapped in {{ }}, remove the {{ }}.
  if (`{{` &&
      rlang::is_call(expr, "{", 1L) &&
      rlang::is_call(expr[[2L]], "{", 1L) &&
      is.symbol(expr[[2L]][[2L]])) {
    expr <- expr[[2L]][[2L]]
  }
  rlang::as_label(expr)
}





# A function that generates a unique name that is not one of the elements of vec
# nor the name of any object in env nor any of its parents/grandparents
make_unique_name <- function(x, env = NULL, vec = character()) {
  # As long as the last element of x matches an element or the name of an
  # object in env (or its enclosing frames)...
  while (any(vec == x[length(x)]) ||
         !is.null(env) && exists(x[length(x)], envir = env, inherits = TRUE)) {
    # ...append a copy of the first element of x to the end of x. Run
    # make.names(unique = TRUE) again in the hopes that it will be changed to
    # something unique.
    x <- make.names(x[c(seq_along(x), 1L)], unique = TRUE)
  }
  # If we've gotten here, the last element of x doesn't exist in vec, env, or
  # its enclosing frames
  x[length(x)]
}



# Wrap sym in a call to as.factor(), as.ordered(), or factor() if it does not
# evaluate to a factor (or ordered factor, for as.ordered()).
make_factor_call <- function(sym,
                             fn = c("as.factor", "as.ordered"),
                             data,
                             env) {
  fn <- match.arg(fn)

  # Evaluate sym and grab its values.
  values <- eval(sym, data, env)

  predicate <- switch(fn, as.factor = is.factor, as.ordered = is.ordered)
  if (!predicate(values)) sym <- call(fn, sym)

  sym
}



# Obtain the long form covariance structure name of an mmrm fit.
get_cov_type_long <- function(fit) {
  cov_type_short <- mmrm::component(fit, "cov_type")
  cov_type_long <-
    c(us = "heterogeneous unstructured",
        toep = "homogeneous Toeplitz",
        toeph = "heterogeneous Toeplitz",
        ar1 = "homogeneous autoregressive order 1",
        ar1h = "heterogeneous autoregressive order 1",
        csh = "heterogeneous compound symmetry",
        cs = "homogeneous compound symmetry",
        ad = "homogeneous ante-dependence",
        adh = "heterogeneous ante-dependence",
        sp_exp = "spatial exponential"
      )[cov_type_short]
  cov_type_long <- unname(cov_type_long)
  cov_type_long
}
