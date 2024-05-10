#' Calculate the effective sample size, per chain and in total, of draws from an MCMC algorithm
#'
#' @param mhDraws List of data.frames. Each data.frame represents a single chain. Data.frame columns for which ESS is calculated should be named val.1, ..., val.k
#' @param ... Catches unnecessary additional arguments
#'
#' @return Data.frame with 1 Row and (# Chains + 1) Columns. Each entry gives the estimated ESS for the chain or sum of chains.
#'
sess <- function(mhDraws, ...){
  #Strip out unecessary info from mhDraws
  mhMCMC <- lapply(mhDraws, function(df){
    return(df[grepl('val', names(df))])
  })

  #Calculate effective sample size
  chainESS <- sapply(mhMCMC, mcmcse::multiESS)

  #Present as df and return
  chainESS <- as.data.frame(matrix(chainESS, nrow = 1))
  names(chainESS) <- paste0('Chain ', 1:length(mhDraws))
  chainESS$Sum <- rowSums(chainESS)
  row.names(chainESS) <- 'Effective Sample Size'

  return(chainESS)
}
