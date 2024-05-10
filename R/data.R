#' Results from a univariate MCMC algorithm on a simulated posterior
#'
#' Results from a univariate Metropolis-Hastings algorithm run on a tri-modal posterior.
#' Although the standard traceplot and Gelman-Rubin diagnostic show good mixing, the results
#' are actually mixing poorly. Included for examples.
#'
#' @format ## `uniMCMCResults`
#' A list with 7 elements, each representing a different MCMC chain. Each element is
#' a numeric vector of length 2000
#' @source Luke Duttweiler
"uniMCMCResults"

#' Results from a Bayesian Network MCMC algorithm on simulated data
#'
#' Results from a Bayesian Network Metropolis-Hastings algorithm run on simulated data.
#' Included for examples.
#'
#' @format ## `bnMCMCResults`
#' A list with 5 elements, each representing a different MCMC chain. Each element is
#' a list of data.frames describing a partition arrangement of a Bayesian Network.
#' @source Luke Duttweiler
"bnMCMCResults"
