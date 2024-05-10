#' Uses generic formulas and rough time estimate to estimate time it will take to evaluate
#' the TS algorithm on a set of unique draws with the tsTransform function.
#'
#' @param distance Function with two parameters x,y. Used to calculate distance between
#'  draw1 and draw2
#' @param draw1 Object that works as an argument for distance()
#' @param draw2 Different object that works as an argument for distance()
#' @param N Number of unique draws for which the user is interested in evaluating
#' the time to completion for the TS algorithm
#'
#' @return Data.frame with 1 row and 2 columns. Entry one gives the standard completion time,
#' entry two gives the completion time if the fuzzy approximation is used.
#'
estimateTsTime <- function(distance, draw1, draw2, N){
  #Calculate time it takes to evaluate one instance of distance
  t1 <- Sys.time()
  mork <- distance(draw1, draw2)
  t2 <- Sys.time()

  oneTime <- t2 - t1

  #Standard estimate
  standardTime <- oneTime*(N)*(N-1)/2

  #Fuzzy Estimate
  fuzzyTime <- oneTime*(N*51 + (N^2)/200 - N/2 + 50*99)

  return(data.frame(Standard = round(standardTime/60), Fuzzy = round(fuzzyTime/60)))
}
