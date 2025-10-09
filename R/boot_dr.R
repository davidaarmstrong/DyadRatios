#' Dyad Ratios Algorithm
#'
#' Estimates the Dyad Ratios Algorithm for constructing latent time series from
#' survey-research marginals.
#'
#' @param varname String giving the name of the input series to be smoothed.
#'   This should identify similar or comparable values in the series. Values in
#'   the series that have the same \code{varname} will be assumed to come from
#'   the same source.
#' @param date ISO numeric representation of the date the survey was in the
#'   field (usually the start, end, or median date).
#' @param index Numeric value of the series. It might be a percent or proportion
#'   responding in a single category (e.g., the approve response in presidential
#'   approval) or some multi-response summary. For ease of interpretation,
#'   polarity should be the same for all items.
#' @param ncases Number of cases (e.g., sample size) of the survey. This provides
#'   differential weighting for the values. Setting this to \code{NULL} or
#'   leaving it blank will weight each value equally.
#' @param unit Aggregation period—one of \sQuote{D} (daily), \sQuote{M}
#'   (monthly), \sQuote{Q} (quarterly), \sQuote{A} (annual), or \sQuote{O}
#'   (multi-year aggregation).
#' @param mult Number of years, only used if \code{unit} is \sQuote{O}.
#' @param begindt Beginning date of the analysis. Defaults to earliest date in
#'   the dataset. Should be specified with \code{lubridate::ymd()}.
#' @param enddt Ending date for the analysis. Defaults to the latest date in the
#'   data.
#' @param npass Not yet implemented.
#' @param smoothing Logical. Specifies whether exponential smoothing is applied
#'   to the intermediate estimates during the iterative solution process.
#'   Defaults to \code{TRUE}.
#' @param endmonth Ending month of the analysis.
#' @param R Number of bootstrap samples.
#' @param parallel Logical indicating whether the `mclapply` function should be used. 
#' @param ... Other arguments to be passed down to `mclapply`.  
#'
#' @return A data frame with variables:
#' \itemize{
#'   \item \code{period}: Aggregation period. 
#'   \item \code{latent1}: Estimate of latent variable from original analysis.
#'   \item \code{lwr}: Lower confidence bound.
#'   \item \code{upr}: Upper confidence bound. 
#' }
#'
#' @references
#' Stimson, J. A. (2018).
#' \sQuote{The Dyad Ratios Algorithm for Estimating Latent Public Opinion:
#' Estimation, Testing, and Comparison to Other Approaches},
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie
#' Sociologique}, 137–138(1), 201–218. \doi{10.1177/0759106318761614}
#'
#' @importFrom stats cor lm optim sd var quantile aggregate rbinom
#' @importFrom graphics legend lines par plot 
#' @importFrom lubridate ymd year month day quarter
#' @importFrom progress progress_bar
#' 
#' @usage NULL
#' @export
#'
#' @examples
#' data(jennings)
#' # R should be higher for real-world applications
#' boot_out <- boot_dr(varname = jennings$variable, 
#'                   date = jennings$date, 
#'                   index = jennings$value, 
#'                   ncases = jennings$n, 
#'                   begindt = min(jennings$date), 
#'                   enddt = max(jennings$date), 
#'                   npass=1, R=50, parallel=FALSE)
#' boot_out
#' 

boot_dr <- function(varname,
                    date,
                    index,
                    ncases=NULL,
                    unit="A",
                    mult=1,
                    begindt=NA,
                    enddt=NA,
                    npass=1,
                    smoothing=TRUE,
                    endmonth=12, 
                    R=1000, 
                    parallel=FALSE, 
                    ...) { 
  if(is.null(ncases))ncases <- rep(1000, length(index))
  if(!is.null(ncases) & any(is.na(ncases)))ncases <- ifelse(is.na(ncases), 1000, ncases)
  if(any(index > 1))index <- index/100
  e_orig <- DyadRatios::extract(varname, date, index, ncases, unit, mult, begindt, enddt, npass, smoothing, endmonth)
  if(!parallel){
    pb <- progress_bar$new(total = R)
    moods <- vector(mode="list", length=R)
    for(iter in 1:R){
      n_cases_boot <- rbinom(n = length(index), prob= index, size=ncases)
      index_boot <- n_cases_boot/ncases
      e <- DyadRatios::extract(varname, date, index_boot, ncases, unit, mult, begindt, enddt, npass, smoothing, endmonth)
      moods[[iter]] <- get_mood(e)
      pb$tick()
    }
  }else{
    if(!requireNamespace("parallel", quietly = TRUE)){
      stop("Must install parallel package if parallel=TRUE.\n")
    }
    moods <- parallel::mclapply(1:R, \(i){
      n_cases_boot <- rbinom(n = length(index), prob= index, size=ncases)
      index_boot <- n_cases_boot/ncases
      e <- DyadRatios::extract(varname, date, index_boot, ncases, unit, mult, begindt, enddt, npass, smoothing, endmonth)
      get_mood(e)
    }, ...)
    
  }
  make_qtl <- function(x){
    data.frame(lwr = c(unname(quantile(x, .025))), 
               upr = c(unname(quantile(x, .975))))
  }
  moods <- as.data.frame(do.call(rbind, moods))
  lwr <- stats::aggregate(moods$latent1, list(moods$period), 
                   \(x)c(unname(quantile(x, .025))))
  upr <- stats::aggregate(moods$latent1, list(moods$period), 
                   \(x)c(unname(quantile(x, .975))))
  names(lwr) <- c("period", "lwr")
  names(upr) <- c("period", "upr")
  cis <- merge(lwr, upr)
  res <- merge(get_mood(e_orig), cis)
  class(res) <- c("boot_dr", "data.frame")
  res
}

