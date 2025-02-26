% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genDiagnostic.R
\name{genDiagnostic}
\alias{genDiagnostic}
\title{Generate Generalized Diagnostics for Markov Chain Monte Carlo Draws}
\usage{
genDiagnostic(
  mhDraws,
  proximityMap = c("standard", "ts", "lanfear"),
  diagnostics = c("traceplot", "ess", "psrf"),
  distance = NULL,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{mhDraws}{A list of MCMC draws, where each element is an ordered list or numeric vector representing the output of a single MCMC chain.}

\item{proximityMap}{Method (called a proximity-map) for transforming the MCMC draws. Options include 'standard', 'ts', 'lanfear', or a custom function. See details.}

\item{diagnostics}{A character vector or list of diagnostic functions to be evaluated. Options include 'traceplot', 'ess', 'psrf', or custom functions. See details.}

\item{distance}{Function for evaluating distance between MCMC draws if required by 'method'. This should be a pairwise distance function that operates on elements of the chains from mhDraws. Note that the lanfear and ts proximityMaps ALWAYS require a distance function.}

\item{verbose}{If TRUE, informative messages are displayed.}

\item{...}{
  Arguments passed on to \code{\link[=tsTransform]{tsTransform}}, \code{\link[=lanfearTransform]{lanfearTransform}}
  \describe{
    \item{\code{minDist}}{Numeric. Value which specifies the minimum possible distance for two draws
which are not equal. See tsTransform details.}
    \item{\code{fuzzy}}{Logical. If TRUE computes an approximate version of the TS algorithm.
See tsTransform details.}
    \item{\code{fuzzyDist}}{Numeric. Parameter for approximate version of ts algorithm. See tsTransform details.}
    \item{\code{reference}}{Argument for method = 'lanfear'. Reference point for lanfearTransform (with exact same structure as each MCMC draw)
for draw comparison. If left NULL a random point is selected from the given draws.
See lanfearTransform details.}
  }}
}
\value{
An object of class 'mcmcDiag', containing evaluated diagnostics, transformed draws, and function call details.
}
\description{
This function generates generalized diagnostics for Markov Chain Monte Carlo (MCMC) draws, transforming the draws if specified, and evaluating selected diagnostics.
}
\details{
Built-in proximity-maps can be called with the appropriate character string in the 'proximity-map'
argument. For details on a particular proximity-map use ?lanfearTransform or ?tsTransform,
the standard proximity-map induces no transformation. Custom proximity-map
functions may be added as well. A custom function must be written to accept a list of mcmcChain
type objects, and output a list of dataframes with columns val (the transformed draw) and t (the MCMC chain order).
Each element in the list is the transformed MCMC chain corresponding to the input.

Built-in diagnostics can be called with the appropriate character string in the 'diagnostics'
argument. Current diagnostic options are 'traceplot' for traceplots, 'ess' for Effective Sample Size,
and 'psrf' for the Gelman-Rubin Potential Scale Reduction Factor.
Additional custom diagnostic functions may be written. These functions should
act on a list of data.frames output from a transform function and should output as a relatively
small data.frame where the name of diagnostic is the first row.name.
}
\examples{
#Example using standard Traceplot
tstS <- genDiagnostic(uniMCMCResults)
tstS

#Example using 'lanfear' traceplot
tstL <- genDiagnostic(uniMCMCResults, proximityMap = 'lanfear', distance = eucDist,
                      reference = 0)
tstL

#Example using bayesian network sample data, with 'lanfear' proximityMap
tstBN1 <- genDiagnostic(bnMCMCResults, proximityMap = 'lanfear', distance = partitionDist)
tstBN1

}
