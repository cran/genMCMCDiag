#Line to get CRAN to accept package
if(getRversion() >= "2.15.1")  utils::globalVariables(c('A'))

#' Euclidean Distance
#'
#' Simple function to return the Euclidean distance between two objects. Acts elementwise.
#'
#' @param x Numeric vector or matrix.
#' @param y Numeric vector or matrix of same dimensions as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions, take care!
#'
#' @return Numeric, elementwise Euclidean distance between x and y.
#' @export
#'
#' @examples
#' eucDist(c(0,0), c(1,1))
eucDist <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

#' Hamming Distance
#'
#' Simple function to return the Hamming distance between two objects. Acts elementwise.
#'
#' @param x Binary vector or matrix
#' @param y Binary vector or matrix of same dimensions as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions. Also,
#' does not test to make sure x,y are binary, take care!
#'
#' @return Numeric, elementwise Hamming distance between x and y.
#' @export
#'
#' @examples
#' x <- matrix(c(1,0,
#'               0,0), nrow = 2, byrow = TRUE)
#' y <- diag(1,2)
#' hammingDist(x, y)
hammingDist <- function(x,y){
  return(sum(abs(x-y)))
}

#' Partition Distance
#'
#' Function to return the 'Partition' distance between two objects. Used for Bayesian
#' Networks with the 'partition-MCMC' algorithm.
#'
#' @param x Data.frame with columns node and partition
#' @param y Data.frame with columns node and partition. Same nrows as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions. Also,
#' does not test to make sure x,y are data.frames of integers, take care!
#'
#' @return Numeric, Partition distance between x and y.
#' @export
#'
#' @examples
#' x <- bnMCMCResults[[1]][[1]]
#' y <- bnMCMCResults[[1]][[100]]
#' partitionDist(x, y)
partitionDist <- function(x, y){
  #Sort by nodes
  x <- x[order(x$node),]
  y <- y[order(y$node),]

  #Take absolute difference of partition levels
  return(sum(abs(x$partition - y$partition)))
}

#' DPMM Distance
#'
#' For an MCMC draw from a DPMM D_x, let Z_x be the vector of Z-scores of the
#' observations based on that observation's current group, and let A_x be the
#' 0,1 adjacency matrix where \deqn{[A_x]_{ij} = 1} if observations i and j are in
#' the same group in draw D_x (so the diagonal is always 1s).
#' Then we define the DPMM distance between D_x and D_y as: \deqn{
#'    d(D_x, D_y) = |Z_x - Z_y|^T(|A_x - A_y| + I)|Z_x - Z_y|.
#' }
#'
#' @param x List with elements 'Zscore' and 'Adj'
#' @param y List with elements 'Zscore' and 'Adj', both of same dimensions as in x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions. The function
#' will break if 'Zscore' or 'Adj' doesn't exist though.
#'
#' @return Numeric, DPMM distance between x and y.
#'
dpmmDistance <- function(x, y){
  #Extract elements
  Zx <- x$Zscore
  Zy <- y$Zscore
  Ax <- x$Adj
  Ay <- y$Adj

  #Make distance and return
  Zdiff <- abs(Zx - Zy)
  Adiff <- abs(Ax - Ay) + diag(1, nrow(A))

  return(t(Zdiff) %*% Adiff %*% Zdiff)
}
