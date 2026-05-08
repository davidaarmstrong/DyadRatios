#' dyadratios: Dyad Ratios Algorithm for Latent Public Opinion Estimation
#'
#' @description
#' Implements the Dyad Ratios algorithm (Stimson 1991) for estimating latent
#' public mood from a collection of time-series survey marginals.  The
#' computationally intensive estimation loop is written in C++ (via Rcpp) and
#' is a faithful translation of James Stimson's original 'MCalc' program.  The R
#' layer handles data ingestion, temporal aggregation, result formatting, and
#' visualisation.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{extract}}}{Run the algorithm on a data frame.}
#' }
#'
#' @section Input data format:
#' The primary input is a data frame where each row is one survey marginal:
#' \itemize{
#'   \item A column identifying the opinion \strong{variable} (issue series).
#'   \item A \strong{date} column (any format coercible by \code{as.Date}).
#'   \item An \strong{index} column: the survey proportion or percentage.
#'   \item An optional \strong{n} column: number of respondents (used as
#'         weight; defaults to 1000 if omitted).
#' }
#'
#' @references
#' Stimson, J. A. (1991). \emph{Public Opinion in America: Moods, Cycles, and
#' Swings}. Boulder, CO: Westview Press.
#'
#' Stimson, J. A. (1999). \emph{Public Opinion in America}, 2nd ed. Westview.
#'
#' @docType package
#' @name dyadratios-package
#' @useDynLib DyadRatios, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats rbinom quantile
#' @importFrom graphics polygon
"_PACKAGE"
