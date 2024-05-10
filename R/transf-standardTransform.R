#' Transforms a list of MCMC chains into a list of dataframes with no modifications to values
#'
#' @param mhDraws An list of numeric vectors
#' @param ... Not used.
#'
#' @return A list of data.frames with rows that represent MCMC draws.Each separate data.frame is a
#'  different chain. Data.frames have columns 'val' for the numeric draws, and 't' for the
#'  draw. Currently, using the standard transformation on anything other than univariate
#'  draws is not supported.
#'
standardTransform <- function(mhDraws, ...){
  #Check to make sure this will work
  if(!is.numeric(mhDraws[[1]])){
    stop('The chains must be numeric vectors in order to use the standard transformation.')
  }

  #Will try to force into dataframe of the expected type
  mhRet <- lapply(mhDraws, function(mhChain){
    mhDF <- data.frame(val = mhChain, t = 1:length(mhChain))
    return(mhDF)
  })

  return(mhRet)
}
