#' Helpful mini function to fit the nearest neighbor (NN) algorithm given a set and
#' defined distance
#'
#' @param uniqueDraws List of unique values that make up the set on which we are using
#'  the NN algorithm
#' @param uniqueLabels List of unique labels associated 1-1 with the unique values
#' @param distance Function with arguments x,y that returns a distance
#'  defined on the given values
#' @param minDist Minimum possible distance between two points that aren't equivalent.
#' May be ignored, but if possible to specify, may speed up the algorithm.
#'
#' @return List. tsSolution gives the ordered labels, tsValues gives the ordered values,
#' tsDiffs is a vector of distances between consecutive values in tsValues
fitNN <- function(uniqueDraws, uniqueLabels, distance, minDist){
  #Exit early if uniqueDraws is of length 1
  if(length(uniqueDraws) == 1){
    return(list(tsSolution = uniqueLabels,
                tsValues = uniqueDraws,
                tsDiffs = 0))
  }

  #Homegrown nn solution. Hopefully faster than TSP package
  unusedLabels <- 1:length(uniqueDraws)
  uniqueDrawsSTABLE <- uniqueDraws
  j <- 1 #Start at beginning
  tsSolution <- rep('0', length(uniqueLabels)) #vector for solution
  for(k in 1:(length(uniqueLabels)-1)){
    #Assign label for next step in solution
    tsSolution[k] <- unusedLabels[j]

    #Save current draw for measuring distance
    currentDraw <- uniqueDraws[[j]]

    #Remove current as an option
    unusedLabels <- unusedLabels[-j]
    uniqueDraws <- uniqueDraws[-j]

    #Test distances quitting if we ever hit the minimum distance
    currentDist <- rep(Inf, length(unusedLabels))
    for(t in 1:length(currentDist)){
      d <- distance(uniqueDraws[[t]], currentDraw)
      currentDist[t] <- d
      if(d == minDist){
        break
      }
    }

    #Set the new best move as j
    j <- which(currentDist == min(currentDist))[1]
  }

  #Set final solution
  tsSolution[length(tsSolution)] <- unusedLabels[j]
  names(tsSolution) <- tsSolution

  #Adjust final solution to match correct labels
  finalSolution <- uniqueLabels[as.numeric(tsSolution)]
  names(finalSolution) <- finalSolution

  #Get real values for tsSolution
  tsValues <- uniqueDrawsSTABLE[as.numeric(tsSolution)]

  #Get differences between consecutive values. Line up so that tsDiffs[i] = distance(tsValues[[i]], tsValues[[i+1]])
  #This wraps around as if it were a cycle (thats why the %% length thing exists)
  tsDiffs <- sapply(1:length(tsValues), function(k){
    return(distance(tsValues[[k]], tsValues[[((k) %% length(tsValues)) + 1]]))
  })

  #Determine cut-point (based on which values are farthest apart, NOT MCMC chains)
  #MCMC chain-based cut-point is dealt with in the tsTransform function
  cp <- which(tsDiffs == max(tsDiffs))[1]

  #set cut-point if its not already
  if(cp != length(finalSolution)){
    finalSolution <- c(finalSolution[(cp+1):length(finalSolution)],finalSolution[1:cp])
    tsValues <- c(tsValues[(cp+1):length(tsValues)], tsValues[1:cp])
    tsDiffs <- c(tsDiffs[(cp+1):length(tsDiffs)], tsDiffs[1:cp])
  }

  return(list(tsSolution = finalSolution, tsValues = tsValues, tsDiffs = tsDiffs))
}
