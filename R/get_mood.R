#' Get Mood Estimates from Dyad Ratios Algorithm
#' 
#' Retrieve the latent variable estimates from the dyad ratios algorithm produced by the `extract()` function.
#' @param out An object of class "extract", typically a result of the `extract()` function.
#' @param ... Other arguments to be passed down (currently unused).
#' @return A data frame containing the period of aggregation and the corresponding latent variable estimates for each dimension. 
#' @export
#' @usage NULL
get_mood <- function(out, ...){
  res <- data.frame(period = out$period, 
                    latent1 = out$latent1)
  if(out$dimensions == 2){
    res$latent2 <- out$latent2
  }
  return(res)
}

