#' Generate Generalized Diagnostics for Markov Chain Monte Carlo Draws
#'
#' This function generates generalized diagnostics for Markov Chain Monte Carlo (MCMC) draws, transforming the draws if specified, and evaluating selected diagnostics.
#'
#' @param mhDraws A list of MCMC draws, where each element is an ordered list or numeric vector representing the output of a single MCMC chain.
#' @param proximityMap Method (called a proximity-map) for transforming the MCMC draws. Options include 'standard', 'ts', 'lanfear', or a custom function. See details.
#' @param diagnostics A character vector or list of diagnostic functions to be evaluated. Options include 'traceplot', 'ess', 'psrf', or custom functions. See details.
#' @param distance Function for evaluating distance between MCMC draws if required by 'method'. This should be a pairwise distance function that operates on elements of the chains from mhDraws. Note that the lanfear and ts proximityMaps ALWAYS require a distance function.
#' @param verbose If TRUE, informative messages are displayed.
#' @inheritDotParams tsTransform minDist fuzzy fuzzyDist
#' @inheritDotParams lanfearTransform reference
#'
#' @return An object of class 'mcmcDiag', containing evaluated diagnostics, transformed draws, and function call details.
#'
#' @details
#' Built-in proximity-maps can be called with the appropriate character string in the 'proximity-map'
#' argument. For details on a particular proximity-map use ?lanfearTransform or ?tsTransform,
#' the standard proximity-map induces no transformation. Custom proximity-map
#' functions may be added as well. A custom function must be written to accept a list of mcmcChain
#' type objects, and output a list of dataframes with columns val (the transformed draw) and t (the MCMC chain order).
#' Each element in the list is the transformed MCMC chain corresponding to the input.
#'
#' Built-in diagnostics can be called with the appropriate character string in the 'diagnostics'
#' argument. Current diagnostic options are 'traceplot' for traceplots, 'ess' for Effective Sample Size,
#' and 'psrf' for the Gelman-Rubin Potential Scale Reduction Factor.
#' Additional custom diagnostic functions may be written. These functions should
#' act on a list of data.frames output from a transform function and should output as a relatively
#' small data.frame where the name of diagnostic is the first row.name.
#'
#'
#'
#' @export
#'
#' @examples
#' #Example using standard Traceplot
#' tstS <- genDiagnostic(uniMCMCResults)
#' tstS
#'
#' #Example using 'lanfear' traceplot
#' tstL <- genDiagnostic(uniMCMCResults, proximityMap = 'lanfear', distance = eucDist,
#'                       reference = 0)
#' tstL
#'


#' #Example using bayesian network sample data, with 'lanfear' proximityMap
#' tstBN1 <- genDiagnostic(bnMCMCResults, proximityMap = 'lanfear', distance = partitionDist)
#' tstBN1
#'
genDiagnostic <- function(mhDraws,
                          proximityMap = c('standard', 'ts', 'lanfear'),
                          diagnostics = c('traceplot', 'ess', 'psrf'),
                          distance = NULL,
                          verbose = FALSE,
                          ...){
  #Catch arguments, except for full draws
  argg <- as.list(environment())[-1]
  if(is.character(argg$proximityMap)){
    argg$proximityMap <- argg$proximityMap[1]
  }
  proximityMapArgs <- list(...)

  #Make sure mhDraws is of the correct format
  if(!is.list(mhDraws)){
    stop('mhDraws must be a list with length equal to the number of chains')
  }
  #mhDraws <- lapply(mhDraws, as.list)

  #Make sure diagnostics is not empty
  if(length(diagnostics) == 0){
    stop('diagnostics must contain at least one character or function.')
  }

  #Make sure every entry in diagnostics is in the correct format, create names
  diagNames <- sapply(diagnostics, function(d){
    if(!is.function(d)){
      if(!(d %in% c('traceplot', 'ess', 'psrf'))){
        stop("each diagnostic must be a function or one of 'traceplot','ess','psrf'.")
      }
      #If not a function and a character, use d as name
      return(d)
    }else{
      return('custom')
    }
  })

  #Warnings regarding distance
  if(is.function(proximityMap) & !is.null(distance)){
    if(verbose){
      message('proximityMap is a custom function and may ignore the specified distance.')
    }else{
      warning('proximityMap is a custom function and may ignore the specified distance.')
    }
  }else if(is.character(proximityMap)){
    if(!(proximityMap[1] %in% c('ts', 'lanfear')) & !is.null(distance)){
      if(verbose){
        message('this proximityMap ignores the argument distance.')
      }else{
        warning('this proximityMap ignores the argument distance.')
      }
    }else if((proximityMap[1] %in% c('ts', 'lanfear')) & is.null(distance)){
      stop('distance must be specified for this proximityMap.')
    }
  }


  #Select proximityMap
  if(is.character(proximityMap)){ #If proximityMap is character, must be one of the options
    #Make sure proximityMap is used correctly
    if(!(proximityMap[1] %in% c('standard', 'ts', 'lanfear'))){
      stop("proximityMap must be a function or one of 'standard', 'ts', 'lanfear'.")
    }

    proximityMapF <- get(paste0(proximityMap[1], 'Transform'))
  }else if(is.function(proximityMap)){#If proximityMap is function change call name
    proximityMapF <- proximityMap
  }else{
    stop("proximityMap must be a function or one of 'standard', 'ts', 'lanfear'.")
  }

  #Get transformed data
  mhTransformed <- proximityMapF(mhDraws = mhDraws, distance = distance, verbose = verbose, ...)

  #Select and evaluate diagnostics
  diagRet <- lapply(diagnostics, function(d){
    if(is.character(d)){#If diagnostic is character, fetch correct diagnostic
      diagF <- get(paste0('s', d))
    }else if(is.function(d)){#If diagnostic is function, change name
      diagF <- d
    }else{
      stop("each diagnostic must be a function or one of 'traceplot','ess','psrf'.")
    }

    #Return evaluated diagnostic
    return(diagF(mhTransformed, method = proximityMap))
  })
  names(diagRet) <- diagNames

  retObj <- list(diagnostics = diagRet, transformedDraws = mhTransformed,
                 call = list(arguments = argg, proximityMapArguments = proximityMapArgs))
  class(retObj) <- c('mcmcDiag', 'list')

  return(retObj)
}
