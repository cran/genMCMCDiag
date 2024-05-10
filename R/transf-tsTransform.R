#' Transforms a list of MCMC chains into a list of data.frames using the TS transformation
#'
#' @param mhDraws List. Each element is a single chain from an MCMC algorithm. Each element should be a numeric vector (for univariate draws), or a list.
#' @param distance Distance function defined on the space of MCMC draws. Should operate pairwise on the elements of the given chains. See details.
#' @param minDist Numeric. Value which specifies the minimum possible distance for two draws
#' which are not equal. See tsTransform details.
#' @param fuzzy Logical. If TRUE computes an approximate version of the TS algorithm.
#'  See tsTransform details.
#' @param fuzzyDist Numeric. Parameter for approximate version of ts algorithm. See tsTransform details.
#' @param verbose Logical. If TRUE, function prints out information about approximate
#'  computation time
#' @param ... Catches extra arguments. Not used.
#'
#' @details
#' The TS transformation sets up a traveling salesman algorithm by calculating the
#' pair-wise distances between each unique draw from the mhDraws and solving the resulting
#' TS problem with the nearest neighbor (NN) algorithm.
#'
#' minDist can be used to speed up the algorithm if it is known that
#' when x != y then distance(x, y) >= minDist. Otherwise this should be ignored.
#'
#' The fuzzy approximation of the algorithm works by splitting the unique draws into
#' smaller sets each containing at most 1% of all unique draws,
#' and fitting the NN algorithm within each set, and then on the resulting 'end points'
#' of each set. The sets are created by randomly selecting a representative draw and then
#' putting the 'closest' draws with distance less than fuzzyDist into that set,
#' until the set contains 1% of all unique draws. The fuzzy approximation can GREATLY
#' reduce computation time, unless the fuzzyDistance specified is too small.
#'
#' @return List of data.frames with columns 'val' which is the TS transformation of each
#'  MCMC draw, and 't' which gives the within-chain ordering of the MCMC draws.
#' Each data.frame is a separate chain.
#'
tsTransform <- function(mhDraws, distance, minDist = 0,
                        fuzzy = FALSE, fuzzyDist = 1, verbose = FALSE, ...){

  #Combine one level
  allDraws <- do.call('c', mhDraws)

  #Make labels for all draws. Draws that are identical have the same label
  allDrawLabels <- listLabels(allDraws)

  #If multiple chains...
  if(length(mhDraws) > 1){
    #Set labels for each chain (NOTE: assuming all chains have same number of draws)
    chainLabels <- split(allDrawLabels, cut(seq_along(allDrawLabels), length(mhDraws)))
    names(chainLabels) <- 1:length(mhDraws)
  }else{ #Otherwise...
    #Chain Labels are just the allDrawLabels
    chainLabels <- list(allDrawLabels)
    names(chainLabels) <- '1'
  }

  #Get unique draws
  uniqueDraws <- allDraws[!duplicated(allDraws)]
  uniqueDrawsSTABLE <- uniqueDraws #version of unique draws that is unchanged throughout
  uniqueLabels <- allDrawLabels[!duplicated(allDraws)]

  #Estimate times
  timeEst <- estimateTsTime(distance = distance, draw1 = uniqueDraws[[1]],
                            draw2 = uniqueDraws[[2]], N = length(uniqueDraws))

  #if selected is standard and fuzzy is significantly better, warn, otherwise just print
  #Only prints out if verbose
  if(!fuzzy & verbose){
    if(timeEst$Standard > 10*timeEst$Fuzzy){
      message(paste0('Currently using standard calculation which will take approximately ',
                     timeEst$Standard,
                     ' minutes, while fuzzy calculation will take approximately ',
                     timeEst$Fuzzy, ' minutes'))
    }else{
      message(paste0('Estimated time is approximately ',timeEst$Standard,
                     ' minutes. May be faster.'))
    }
  }else if(verbose){
    message(paste0('Estimated time is approximately ',timeEst$Fuzzy,
                   ' minutes. May be faster.'))
  }

  #Create representative bins if fuzzy is requested (speeds things up)
  if(fuzzy){
    #BACKWARD DIRECTION
    binLabels <- c('1',rep('0', length(uniqueLabels)-1))
    binValues <- vector('list', length(uniqueLabels))
    binValues[[1]] <- uniqueDraws[[1]]
    unbinnedValues <- 2:length(uniqueDraws)
    #go through bin values, each time (as long as there is a value) measure the distance
    #Against all REMAINING unbinned values, bin those that belong with the value,
    #skip those that don't
    for(j in 1:length(binValues)){
      #Measure distances
      currentDist <- rep(Inf, length(uniqueDraws))

      for(i in unbinnedValues){
        currentDist[i] <- distance(binValues[[j]], uniqueDraws[[i]])
      }

      valsToBin <- which(currentDist < fuzzyDist)

      #If more than 1% of number of unique vals, limit to 1%
      top <- ceiling(.01*length(uniqueLabels))
      if(length(valsToBin) > top){
        topDist <- sort(currentDist)[top]
        valsToBin <- which((currentDist < fuzzyDist) & currentDist <= topDist)
      }

      #Bin values with distance less than fuzzyDist
      binLabels[valsToBin] <- as.character(j)

      #Remove binned values from unbinnedValues vector
      unbinnedValues <- setdiff(unbinnedValues, valsToBin)

      #If any remain, put next in binValues
      if(length(unbinnedValues) > 0){

        #Select next binValue Label
        if(length(unbinnedValues) == 1){
          nextBin <- unbinnedValues
        }else{
          nextBin <- sample(unbinnedValues,1)
        }

        binLabels[nextBin] <- as.character(j+1)
        unbinnedValues <- setdiff(unbinnedValues, nextBin)
        binValues[[j+1]] <- uniqueDraws[[nextBin]]
      }else{break}
    }

    #Fit nearest neighbor solution within each bin
    nnFits <- lapply(unique(binLabels), function(i){
      tempLabels <- uniqueLabels[binLabels == i]
      tempDraws <- uniqueDraws[binLabels == i]

      ft <- fitNN(tempDraws, tempLabels, distance = distance, minDist = minDist)

      return(ft)
    })

    #Extract bin endpoints
    binEnds <- lapply(nnFits, function(f){
      ends <- f$tsValues[c(1,length(f$tsValues))]
      if(identical(ends[[1]], ends[[2]])){
        return(ends[1])
      }else{return(ends)}
    })


    #Fit final solution using endpoints
    tsSolution <- nnFits[[1]]$tsSolution
    tsValues <- nnFits[[1]]$tsValues
    currentDraw <- tsValues[[length(tsValues)]]
    alreadyUsed <- 1
    for(i in 2:length(nnFits)){ #Loop number of endpoints
      currentDist <- matrix(0, nrow = length(nnFits), ncol = 2)
      for(j in 1:length(nnFits)){
        if(length(binEnds[[j]]) == 1){
          currentDist[j,] <- distance(currentDraw,binEnds[[j]][[1]])
        }else{
          currentDist[j,] <- c(distance(currentDraw, binEnds[[j]][[1]]),
                                distance(currentDraw, binEnds[[j]][[2]]))
        }
      }# End loop over j

      #Make sure already used values aren't chosen
      currentDist[alreadyUsed,] <- Inf

      #select closest value
      nextChoice <- which(currentDist == min(currentDist), arr.ind = TRUE)[1,1]

      #Update alreadyUsed
      alreadyUsed <- c(alreadyUsed, nextChoice)

      #Update tsSolution, tsValues, and currentDraw
      if(currentDist[nextChoice,1] <= currentDist[nextChoice,2]){
        tsSolution <- c(tsSolution, nnFits[[nextChoice]]$tsSolution)
        tsValues <- c(tsValues, nnFits[[nextChoice]]$tsValues)
      }else{
        tsSolution <- c(tsSolution, rev(nnFits[[nextChoice]]$tsSolution))
        tsValues <- c(tsValues, rev(nnFits[[nextChoice]]$tsValues))
      }
      currentDraw <- tsValues[[length(tsValues)]]
    }# End loop over i
    tsSolution <- unlist(tsSolution)
    names(tsSolution) <- tsSolution
  }else{#End if fuzzy
    #Homegrown nn solution. Hopefully faster than TSP package
    unusedLabels <- uniqueLabels
    j <- 1 #Start at beginning
    tsSolution <- rep('0', length(uniqueLabels)) #vector for solution
    for(i in 1:(length(uniqueLabels)-1)){
      #Assign label for next step in solution
      tsSolution[i] <- unusedLabels[j]

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

    #Get real values for tsSolution
    tsValues <- uniqueDrawsSTABLE[as.numeric(tsSolution)]
  }#End if(fuzzy)else()

  #Get differences between consecutive values. Line up so that
  #tsDiffs[i] = distance(tsValues[[i]], tsValues[[i+1]])
  #This wraps around as if it were a cycle (thats why the %% length thing exists)
  tsDiffs <- sapply(1:length(tsValues), function(i){
    return(distance(tsValues[[i]], tsValues[[((i) %% length(tsValues)) + 1]]))
  })

  #Transform chain labels into yaxis values
  mhTSP <- lapply(chainLabels, function(lVec){

    val <- sapply(lVec, function(l){
      which(l == names(tsSolution))
    })

    return(val)
  })

  #Find best cutpoint
  #First find total traceplot travel distance for the different rotations
  overallDists <- sapply(1:length(uniqueLabels), function(r){
    #Rotate Yax by r-1 steps, calculate with true distances
    yAxr <- cumsum(c(0, tsDiffs[((1:length(tsDiffs) + (r-2)) %% length(tsDiffs)) + 1]))

    #Rotate chain by r-1 steps
    mhTSPr <- lapply(mhTSP, function(chain){
      return(yAxr[((chain + (r-2)) %% length(uniqueLabels)) + 1])
    })

    #calculate total distance the line is traveling
    chainDists <- sapply(mhTSPr, function(chain){
      return(sum(abs(diff(chain))))
    })
    totalDist <- sum(chainDists)

    return(totalDist)
  })

  #Get the optimal cutpoint
  optR <- which(overallDists == min(overallDists))[1]

  #Create y-axis based on true distances
  trueYAx <- cumsum(c(0, tsDiffs[((1:length(tsDiffs) + (optR-2)) %% length(tsDiffs)) + 1]))

  #Rotate chains to optimal cutpoint
  mhTSP <- lapply(mhTSP, function(chain){

    chainR <- trueYAx[((chain + (optR-2)) %% length(uniqueLabels)) + 1]

    return(data.frame('val' = chainR, 't' = 1:length(chainR)))
  })

  #Return transformed chains
  return(mhTSP)
}
