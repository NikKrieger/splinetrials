#' Natural Cubic Spline Basis Matrix for Continuous Time.
#'
#' Wrapper around [splines::ns()] with default `Boundary.knots` of `c(0,
#' max(time))`.
#'
#' `time_spline()` is primarily useful because it can create the spline basis
#' from `time` and then re-input `time` into the spline basis to obtain the
#' predictions in one step. Or, it can calculate predictions from a basis
#' supplied to the `basis` argument.
#'
#' @param time Continuous time variable, passed directly to [splines::ns()] as
#'   the first argument.
#' @param df Degrees of freedom, passed directly to the `df` argument of
#'   [splines::ns()].
#' @param Boundary.knots Boundary knots, passed directly to the `Boundary.knots`
#'   argument of [splines::ns()]. Defaults to `c(0, max(time))`.
#' @param ... Passed to [splines::ns()].
#'
#' @returns A matrix of dimension `length(time) * df`. See the *Value* section
#'   of [splines::ns()].
#' @export
#'
#' @examples
#' time_spline_basis(Theoph$Time, df = 3)
time_spline_basis <- function(time, df, Boundary.knots = c(0, max(time)), ...) {
  splines::ns(time, df = df, Boundary.knots = Boundary.knots, ...)
}



#' Create Natural Cubic Spline Approximations for Continuous Time
#'
#' Accepts or constructs a natural cubic spline `basis` for continuous `time`
#' and yields a matrix of approximations for `time` according to that `basis`.
#'
#' `time_spline()` is primarily useful because it can use one step to create the
#' spline `basis` from `time` and then re-input `time` into the spline `basis`
#' to obtain the spline approximations. Alternatively, it can calculate
#' predictions from a basis supplied to the `basis` argument.
#'
#' @param time A numeric vector of values.
#' @param df,... Only used if `basis` is left as the default. Passed to
#'   [time_spline_basis()] (which passes all arguments to [splines::ns()]) to
#'   calculate the spline `basis`.
#' @param basis Spline basis for which to create approximations of `time`.
#'   Defaults to [`time_spline_basis`]`(time, df = df, ...)`.
#'
#' @returns Matrix with the same dimensions as `basis`. Contains `basis` as an
#'   [attr]ibute.
#' @export
#'
#' @examples
#' time_spline(Theoph$Time, df = 3)
#'
#' # Or, compute the spline basis beforehand, and then pass it to time_spline()
#' basis <-
#'   splines::bs(Theoph$Time, df = 3, Boundary.knots = c(0, max(Theoph$Time)))
#'
#' time_spline(Theoph$Time, basis = basis)
time_spline <- function(time,
                        df = NULL,
                        ...,
                        basis = time_spline_basis(time, df = df, ...)) {
  if (!missing(basis) && (!is.null(df) || ...length())) {
    cli::cli_warn(c(
      "Since {.var basis} was supplied, {.var df} and {.var ...} will be ignored.",
      if (!is.null(df)) c("i" = "{.var df} = {df} will be ignored."),
      if (...length()) {
        c("i" = "These elements supplied to {.var ...} will be ignored: {as.list(substitute(...))}")
      }
    ))
  }

  out <- stats::predict(basis, time)
  attr(out, "basis") <- basis
  class(out) <- c("splinetrials_time_spline", class(out))

  out
}
