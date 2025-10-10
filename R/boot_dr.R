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
#' @param level The confidence level for the intervals. Default is 0.95. 
#' @param pw Logical indicating whether to do pairwise tests. 
#' @param ... Other arguments to be passed down to `mclapply`.  
#'
#' @return A list with potentially two data frames `ci` has variables:
#' \itemize{
#'   \item \code{period}: Aggregation period. 
#'   \item \code{latent1}: Estimate of latent variable from original analysis.
#'   \item \code{lwr}: Lower confidence bound.
#'   \item \code{upr}: Upper confidence bound. 
#' }
#' If `pw = TRUE`, the list also contains `pw` with variables:
#' \itemize{
#'   \item \code{p1}: Earlier period 
#'   \item \code{p2}: Later period 
#'   \item \code{diff}: (mood for p2) - (mood for p1)
#'   \item \code{p_diff}: Probability that the larger mood is bigger than the smaller mood. 
#' }
#'
#' @references
#' Stimson, J. A. (2018).
#' \sQuote{The Dyad Ratios Algorithm for Estimating Latent Public Opinion:
#' Estimation, Testing, and Comparison to Other Approaches},
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie
#' Sociologique}, 137–138(1), 201–218. \doi{10.1177/0759106318761614}
#'
#' @importFrom stats cor lm optim sd var quantile aggregate rbinom reshape
#' @importFrom graphics legend lines par plot 
#' @importFrom lubridate ymd year month day quarter
#' @importFrom progress progress_bar
#' @importFrom utils combn
#' 
#' @usage NULL
#' @export
#'
#' @examples
#' data(jennings)
#' # R should be higher for real-world applications
#' \dontrun{
#' boot_out <- boot_dr(varname = jennings$variable, 
#'                   date = jennings$date, 
#'                   index = jennings$value, 
#'                   ncases = jennings$n, 
#'                   begindt = as.Date("1985-01-01"), 
#'                   enddt = max(jennings$date), 
#'                   npass=1, R=1000, 
#'                   parallel=FALSE)
#' boot_out
#' }
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
                    level = .95,
                    pw = FALSE, 
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
      e <- extract_bs(varname, date, index_boot, ncases, unit, mult, begindt, enddt, npass, smoothing, endmonth, wm=e_orig$wtdmean, ws=e_orig$wtdstd)
      out <- get_mood(e)
      out$iter <- iter
      moods[[iter]] <- out
      pb$tick()
    }
  }else{
    if(!requireNamespace("parallel", quietly = TRUE)){
      stop("Must install parallel package if parallel=TRUE.\n")
    }
    moods <- parallel::mclapply(1:R, \(i){
      n_cases_boot <- rbinom(n = length(index), prob= index, size=ncases)
      index_boot <- n_cases_boot/ncases
      e <- extract_bs(varname, date, index_boot, ncases, unit, mult, begindt, enddt, npass, smoothing, endmonth, wm=e_orig$wtdmean, ws=e_orig$wtdstd)
      out <- get_mood(e)
      out$iter <- i
      out
    }, ...)
    
  }
  lwr_level <- (1 - level)/2
  upr_level <- 1 - lwr_level
  moods <- as.data.frame(do.call(rbind, moods))
  if(pw){
    moodsw <- reshape(moods, idvar = "period", timevar = "iter", direction = "wide")
    pd <- moodsw$period
    moods_mat <- t(as.matrix(moodsw[,-1]))
    combs <- combn(ncol(moods_mat), 2)
    D <- matrix(0, nrow=ncol(moods_mat), ncol=ncol(combs))
    D[cbind(combs[1,], 1:ncol(combs))] <- -1
    D[cbind(combs[2,], 1:ncol(combs))] <- 1
    diffs <- moods_mat %*% D
    p_diff <- apply(diffs, 2, \(x)mean(x > 0))
    p_diff <- ifelse(p_diff < .5, 1-p_diff, p_diff)
    diff = matrix(get_mood(e_orig)$latent1, nrow=1) %*% D
    res_pw <- data.frame(p1 = pd[combs[1,]], p2 = pd[combs[2,]], diff = c(diff), p_diff = p_diff)
  }
  lwr <- stats::aggregate(moods$latent1, list(moods$period), 
                   \(x)c(unname(quantile(x, lwr_level))))
  upr <- stats::aggregate(moods$latent1, list(moods$period), 
                   \(x)c(unname(quantile(x, upr_level))))
  names(lwr) <- c("period", "lwr")
  names(upr) <- c("period", "upr")
  cis <- merge(lwr, upr)
  cis <- merge(get_mood(e_orig), cis)
  res <- list(ci = cis)
  if(pw)res[["pw"]] <- res_pw
  return(res)
}

