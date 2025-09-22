#' Jennings Government Trust Data
#'
#' A dataset of survey marginals from the British Social Attitudes (BSA) survey,
#' measuring public trust in government. These marginals are commonly used as
#' input to the Dyad Ratios Algorithm for constructing latent time series.  We 
#' replaced missing sample sizes with a value of 850, which is roughly the minimum
#' sample size observed in the data. 
#'
#' @format A data frame with 4 variables and `r nrow(jennings)` rows:
#' \describe{
#'   \item{variable}{Character string identifying the survey question or series.}
#'   \item{date}{Date the survey was fielded.}
#'   \item{value}{percentage of people indicating distrust in the government.}
#'   \item{n}{Sample size for the survey wave.}
#' }
#'
#' @source Jennings, W. N. Clarke, J. Moss and G. Stoker  (2017). "The Decline in Diffuse Support for National Politics: The Long View on Political Discontent in Britain" In *Public Opinion Quarterly*,
#'  81(3), 748-758. \doi{10.1093/poq/nfx020}
#'
#' @usage NULL
#' @examples
#' data(jennings)
#' head(jennings)
"jennings"

