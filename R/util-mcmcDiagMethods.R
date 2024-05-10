#' Print method for mcmcDiag objects
#'
#' @param x Object of class mcmcDiag
#' @param ... Kept for consistency with print. Does nothing.
#'
#' @return Invisible NULL, prints to console
#' @export
#'
#' @examples
#' print(genDiagnostic(uniMCMCResults))
print.mcmcDiag <- function(x, ...){
                     #Extract method
                     m <- x$call$arguments$method

                     #Change to 'Custom' if custom function was used
                     if(!is.character(m)){
                       m <- 'Custom'
                     }

                     #Extract diagnostics
                     diags <- x$diagnostics

                     #Special handling for traceplot
                     if('traceplot' %in% names(diags)){
                       print(diags$traceplot)
                       diags <- diags[names(diags) != 'traceplot']
                     }

                     if(length(diags) !=0){
                       catStr <- paste0('----------------------------------------------------\n',
                                        'Generalized MCMC Diagnostics using ', m,
                                        ' Method \n----------------------------------------------------\n\n')

                       #Other diagnostics printed
                       for(d in names(diags)){
                         nm <- row.names(diags[[d]])[1]
                         row.names(diags[[d]]) <- NULL
                         strAdd <- paste0('|',nm, ': \n|---------------------------\n',
                                          paste0(knitr::kable(round(diags[[d]], 3)),
                                                 collapse = '\n'),
                                          '\n\n')
                         catStr <- paste0(catStr, strAdd)
                       }
                       cat(catStr)
                     }
                     return(invisible(NULL))
                   }
