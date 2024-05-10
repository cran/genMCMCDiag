#' Function to assign character labels to all unique objects in a list
#'
#' @param lst A list of objects. Each object in the list should have
#' the same general structure
#'
#' @return A character vector of labels. Objects in lst that are identical will be assigned
#' the same label.
listLabels <- function(lst){
  #Much faster if we can use the grouping function, not all versions are supported so we adjust
  if(is.numeric(lst)){
    grs <- grouping(as.character(lst))
    ends <- attr(grs, 'ends')
    indices <- rep(seq_along(ends), c(ends[1], diff(ends)))[order(grs)]
    return(as.character(indices))
  }

  #If not a numeric, go through more complicated version
  dupVals <- duplicated(lst)

  #Create list to save unique values and vector to save unique labels
  uniqueVals <- vector('list',  sum(!dupVals))
  uniqueLabels <- vector('character', sum(!dupVals))

  #Create vector of labels for the values of lst
  lstLabels <- vector('character', length(lst))

  #Index of how many spots in uniqueVals and labels have been filled
  j <- 0

  #Loop over list, different action if duplicated vs not
  for(i in 1:length(dupVals)){
    #If duplicated
    if(dupVals[i]){
      #Index of the original value this is a duplicate of
      origIndex <- which(sapply(uniqueVals, identical, y = lst[[i]]))

      #Assign label
      lstLabels[i] <- uniqueLabels[origIndex]
    }else{ #If not duplicated, add to uniqueVals and add a new label
      #Iterate j (index of where we are in uniqueVals and labels)
      j <- j+1

      #Add value to uniqueVals
      uniqueVals[[j]] <- lst[[i]]

      #Add unique label
      uniqueLabels[j] <- as.character(j)

      #Assign label to final output
      lstLabels[i] <- as.character(j)
    } #End if/else
  }#End Loop

  return(lstLabels)
}
