#' Compute errors of simulation
#'
#' @param par Parameters of \code{sim_fun}.
#' @param sim_fun Simulation function.
#' @param ... Additional arguments passed to \code{sim_fun}.
#' @param obs Observed values.
#' @param err_fun Error function.
#' @export
#' @examples
#' sim_fun <- function(par) {
#'
#' }
sim_error <- function(par, sim_fun, ..., obs, err_fun = rmse) {
  sim_fun <- match.fun(sim_fun)
  err_fun <- match.fun(err_fun)
  sim_fun(par, ...) %>%
    err_fun(obs)
}

#' Error functions
#'
#' \describe{
#'   \item{res}{Residual}
#'   \item{ssq}{Sum of squares}
#'   \item{mse}{Mean squared error}
#'   \item{rmse}{Root-mean-square error}
#'   \item{mae}{Mean absolute error}
#' }
#'
#' @param sim Simulated values.
#' @param obs Observed values.
#' @rdname err_fun
#' @export
#' @examples
#' sim <- 1:2
#' obs <- numeric(2)
#' res(sim, obs)
#' ssq(sim, obs)
#' mse(sim, obs)
#' rmse(sim, obs)
#' mae(sim, obs)
res <- function(sim, obs) {
  sim %>%
    subtract(obs)
}
#' @rdname err_fun
#' @export
ssq <- function(sim, obs) {
  sim %>%
    res(obs) %>%
    raise_to_power(2) %>%
    sum(na.rm = TRUE)
}
#' @rdname err_fun
#' @export
mse <- function(sim, obs) {
  n <- sum(!is.na(sim) & !is.na(obs))
  ssq(sim, obs) %>%
    divide_by(n)
}
#' @rdname err_fun
#' @export
rmse <- function(sim, obs) {
  mse(sim, obs) %>%
    sqrt()
}
#' @rdname err_fun
#' @export
mae <- function(sim, obs) {
  n <- sum(!is.na(sim) & !is.na(obs))
  sim %>%
    res(obs) %>%
    abs() %>%
    sum(na.rm = TRUE) %>%
    divide_by(n)
}

#' Bisquare kernel
#'
#' @param dx Distances from center.
#' @param width Width of kernel.
#' @export
#' @examples
#' bisq(-6:6, 10)
bisq <- function(dx, width) {
  dx %>%
    {(1 - (dx / (width / 2)) ^ 2) ^ 2} %>%
    replace(abs(dx) > (width / 2), 0)
}
