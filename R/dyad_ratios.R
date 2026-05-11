#' Run the Dyad Ratios Algorithm
#'
#' Estimates one or two latent opinion dimensions from a collection of
#' time-series issue variables using the Dyad Ratios algorithm developed by
#' James Stimson (Stimson 1991, 1999).  This function accepts already-loaded
#' data as a \code{data.frame}, handles aggregation to the requested interval,
#' standardises the issue matrix, passes it to the compiled C++ core, and
#' returns a richly-annotated result object.
#'
#' @param data A \code{data.frame} with at minimum the columns specified in
#'   \code{varname_col}, \code{date_col}, \code{index_col}, and optionally
#'   \code{n_col}.  Each row represents one survey marginal: the proportion
#'   (or count) of respondents taking a particular position on one issue at
#'   one point in time.  The variable name column identifies which opinion
#'   item the row belongs to.
#' @param varname_col Character.  Name of the column that identifies the
#'   opinion variable (issue series).  Default \code{"varname"}.
#' @param date_col Character.  Name of the column containing the observation
#'   date.  Must be coercible to \code{Date} via \code{as.Date()}.
#'   Default \code{"date"}.
#' @param index_col Character.  Name of the column containing the survey
#'   marginal value (e.g. proportion liberal, per cent approving, etc.).
#'   Default \code{"index"}.
#' @param n_col Character or \code{NULL}.  Name of the column containing the
#'   number of respondents (sample size / weight).  When \code{NULL} (default)
#'   all observations receive equal weight of 1000, matching the original
#'   program's default.
#' @param agg_interval Character.  Temporal aggregation level.  One of
#'   \code{"annual"} (default), \code{"quarterly"}, \code{"monthly"},
#'   \code{"daily"}, or \code{"multi_year"}.
#' @param multiple Integer.  Number of years per period when
#'   \code{agg_interval = "multi_year"}.  Default \code{1}.
#' @param start_date Optional \code{Date} (or character coercible by
#'   \code{as.Date}) giving the earliest date to include.  Default: the
#'   earliest date found in \code{data}.
#' @param end_date Optional \code{Date} (or character coercible by
#'   \code{as.Date}) giving the latest date to include.  Default: the
#'   latest date found in \code{data}.
#' @param n_dim Integer 1 or 2.  Number of latent dimensions to extract.
#'   Default \code{1}.
#' @param smoothing Logical.  Apply optimal exponential smoothing to each
#'   forward and backward pass.  Default \code{TRUE}.
#' @param tol Numeric.  Convergence tolerance (maximum weighted change in
#'   item-mood correlations between iterations).  Default \code{0.001}.
#' @param fiscal_year_end Integer 1–12.  Final month of the fiscal / policy
#'   year; use 12 (default) for calendar years.  When set to a value < 12
#'   observations after this month are rolled forward into the next year,
#'   consistent with the original program's \code{FinalMonth} parameter.
#' @param smooth_seed Integer.  Seed for the C random number generator used
#'   inside the exponential-smoothing optimiser (\code{eSmooth}).  The default
#'   value of \code{1} replicates MCalc's implicit behaviour: MCalc never
#'   calls \code{srand()}, so C's \code{rand()} starts from seed 1 on every
#'   run.  Change this only if you need to explore sensitivity to the smoothing
#'   optimiser's random fallback step.  Has no effect when
#'   \code{smoothing = FALSE}.
#'
#' @return An object of class \code{"extract"}, which is a named list
#'   containing:
#'   \describe{
#'     \item{\code{mood}}{Numeric vector of estimated public mood, one value
#'       per period.  When \code{n_dim = 2} this is the first dimension.}
#'     \item{\code{mood_dim2}}{Numeric vector of the second dimension, or
#'       \code{NA} when \code{n_dim = 1}.}
#'     \item{\code{periods}}{A \code{data.frame} describing each period
#'       (year, month/quarter where applicable) matching the length of
#'       \code{mood}.}
#'     \item{\code{loadings}}{A \code{data.frame} of variable names with
#'       loading on each extracted dimension.}
#'     \item{\code{iterations}}{A \code{data.frame} of the iteration history
#'       (convergence, reliability, smoothing alphas per iteration).}
#'     \item{\code{n_series}}{Integer.  Number of opinion series retained
#'       after dropping constant series.}
#'     \item{\code{n_periods}}{Integer.  Number of time periods.}
#'     \item{\code{n_obs}}{Integer.  Total number of input observations used.}
#'     \item{\code{alpha_F}}{Final forward-pass smoothing parameter.}
#'     \item{\code{alpha_B}}{Final backward-pass smoothing parameter.}
#'     \item{\code{eigenvalue}}{Eigenvalue estimate for the first dimension.}
#'     \item{\code{variance_explained}}{Proportion of total variance accounted
#'       for by the first dimension.}
#'     \item{\code{variance_explained_dim2}}{(\code{n_dim = 2} only) Additional
#'       proportion of total variance accounted for by the second dimension (i.e.
#'       its share of the variance left unexplained by dimension 1).  \code{NA}
#'       for single-dimension solutions.}
#'     \item{\code{call}}{The matched \code{\link[base]{call}}.}
#'     \item{\code{settings}}{List of all parameter values used.}
#'   }
#'
#' @details
#' The algorithm iterates between a forward pass and a backward pass.  In each
#' pass every period's latent score is estimated as the weighted average of
#' ratios of issue values relative to all other periods for which both values
#' are non-missing.  The weights are the squared correlations of each issue
#' with the current mood estimate.  Convergence is declared when the maximum
#' weighted change in these correlations falls below \code{tol}.
#'
#' Issues are standardised to a mean of 100 and a standard deviation of 10
#' before estimation, then the final mood series is rescaled to have the
#' weighted mean and standard deviation of the raw issue series.
#'
#' @references
#' Stimson, J. A. (1991). \emph{Public Opinion in America: Moods, Cycles, and
#' Swings}. Westview Press.
#'
#' Stimson, J. A. (1999). \emph{Public Opinion in America}, 2nd ed.
#' Westview Press.
#'
#' @examples
#' # Minimal synthetic example
#' set.seed(42)
#' n <- 60
#' dates <- seq(as.Date("1980-01-01"), by = "year", length.out = n / 3)
#' dat <- data.frame(
#'   varname = rep(c("item_a", "item_b", "item_c"), each = length(dates)),
#'   date    = rep(dates, 3),
#'   index   = c(seq(40, 65, length.out = length(dates)) + rnorm(length(dates), 0, 2),
#'               seq(55, 35, length.out = length(dates)) + rnorm(length(dates), 0, 2),
#'               seq(45, 70, length.out = length(dates)) + rnorm(length(dates), 0, 3)),
#'   n       = 1000
#' )
#' result <- extract(dat, n_col = "n", smoothing = FALSE)
#' print(result)
#'
#' @export
extract <- function(data,
                        varname_col    = "varname",
                        date_col       = "date",
                        index_col      = "index",
                        n_col          = NULL,
                        agg_interval   = c("annual", "quarterly", "monthly",
                                           "daily", "multi_year"),
                        multiple       = 1L,
                        start_date     = NULL,
                        end_date       = NULL,
                        n_dim          = 1L,
                        smoothing      = TRUE,
                        tol            = 0.001,
                        fiscal_year_end = 12L,
                        smooth_seed    = 1L) {

  cl <- match.call()
  agg_interval <- match.arg(agg_interval)

  # ---- input validation ---------------------------------------------------
  stopifnot(is.data.frame(data))
  for (col in c(varname_col, date_col, index_col))
    if (!col %in% names(data))
      stop(sprintf("Column '%s' not found in data.", col))
  if (!is.null(n_col) && !n_col %in% names(data))
    stop(sprintf("Column '%s' (n_col) not found in data.", n_col))

  n_dim  <- as.integer(n_dim)
  stopifnot(n_dim %in% 1:2)
  multiple <- as.integer(multiple)
  stopifnot(multiple >= 1)
  fiscal_year_end <- as.integer(fiscal_year_end)
  stopifnot(fiscal_year_end >= 1 && fiscal_year_end <= 12)

  # ---- parse and filter dates --------------------------------------------
  data[[date_col]] <- as.Date(data[[date_col]])
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date))   end_date   <- as.Date(end_date)

  min_date <- if (is.null(start_date)) min(data[[date_col]], na.rm = TRUE) else start_date
  max_date <- if (is.null(end_date))   max(data[[date_col]], na.rm = TRUE) else end_date

  data <- data[!is.na(data[[date_col]]) &
               data[[date_col]] >= min_date &
               data[[date_col]] <= max_date, , drop = FALSE]

  if (nrow(data) == 0)
    stop("No data remain after date filtering.")

  # ---- n (sample size / weight) ------------------------------------------
  if (is.null(n_col)) {
    data$.n_weight <- 1000L
  } else {
    data$.n_weight <- as.numeric(data[[n_col]])
    data$.n_weight[is.na(data$.n_weight) | data$.n_weight == 0] <- 1000L
  }

  # ---- fiscal year adjustment (matches original FinalMonth logic) ----------
  data$.year  <- as.integer(format(data[[date_col]], "%Y"))
  data$.month <- as.integer(format(data[[date_col]], "%m"))
  data$.day   <- as.integer(format(data[[date_col]], "%d"))

  if (fiscal_year_end < 12) {
    roll <- data$.month > fiscal_year_end
    data$.year[roll]  <- data$.year[roll] + 1L
    data$.month[roll] <- 1L
  }

  # ---- derive period index -----------------------------------------------
  data$.period <- .assign_period(data$.year, data$.month, data$.day,
                                 data[[date_col]],
                                 agg_interval, multiple,
                                 min_date, max_date)

  # ---- aggregate to period × variable matrix -----------------------------
  agg <- .aggregate_issues(
    varname      = as.character(data[[varname_col]]),
    index        = as.numeric(data[[index_col]]),
    n            = data$.n_weight,
    period       = data$.period,
    year         = data$.year,
    month        = data$.month,
    agg_interval = agg_interval,
    min_date     = min_date,
    multiple     = multiple
  )

  pIssue_raw <- agg$mat   # nperiods × nVar, 0 = NA sentinel
  var_names  <- agg$var_names
  period_df  <- agg$period_df
  nperiods   <- nrow(pIssue_raw)
  nVar       <- ncol(pIssue_raw)

  if (nVar < 2)
    stop("At least 2 non-constant opinion series are required.")

  # ---- drop constant (zero-variance) series -------------------------------
  keep <- apply(pIssue_raw, 2, function(x) {
    good <- x[x != 0]
    length(good) > 1 && sd(good) > 1e-4
  })
  if (sum(keep) < 2)
    stop("Fewer than 2 non-constant series remain after filtering.")
  pIssue_raw <- pIssue_raw[, keep, drop = FALSE]
  var_names  <- var_names[keep]
  nVar       <- ncol(pIssue_raw)

  # ---- standardise: 100 + 10 * z  (matching original code) ---------------
  # MCalc stores per-variable means as float (32-bit) via av[v]=float(s/nGood),
  # even though av[] is declared double.  Simulate that precision loss so that
  # the standardised values fed to the C++ core match MCalc exactly.
  to_float32 <- function(x)
    readBin(writeBin(as.double(x), raw(), size = 4L),
            double(), n = length(x), size = 4L)

  av_vec  <- numeric(nVar)
  std_vec <- numeric(nVar)
  for (v in seq_len(nVar)) {
    good <- pIssue_raw[, v]; good <- good[good != 0]
    av_vec[v]  <- mean(good)
    std_vec[v] <- sd(good)
  }
  av_vec <- to_float32(av_vec)   # match MCalc's float() cast on the mean

  for (v in seq_len(nVar)) {
    non_na <- pIssue_raw[, v] != 0
    pIssue_raw[non_na, v] <-
      100 + 10 * (pIssue_raw[non_na, v] - av_vec[v]) / std_vec[v]
  }

  # ---- call the C++ estimation core --------------------------------------
  cpp_out <- dyad_ratios_cpp(
    data_matrix = pIssue_raw,
    nperiods    = nperiods,
    nVar        = nVar,
    npass       = n_dim,
    smoothing   = smoothing,
    tola_in     = tol,
    beginper    = 1L,
    endper      = nperiods,
    raw_av      = av_vec,
    raw_std     = std_vec,
    seed        = as.integer(smooth_seed)
  )

  # ---- assemble loadings data frame --------------------------------------
  loadings_df <- data.frame(
    variable = var_names,
    mean     = av_vec,
    std_dev  = std_vec,
    n_obs    = colSums(pIssue_raw != 0),
    dim1     = cpp_out$loadings$dim1,
    stringsAsFactors = FALSE
  )
  if (n_dim == 2)
    loadings_df$dim2 <- cpp_out$loadings$dim2
  row.names(loadings_df) <- NULL

  # ---- build result -------------------------------------------------------
  result <- list(
    mood               = cpp_out$mood,
    mood_dim2          = if (n_dim == 2) cpp_out$mood_dim2 else NA_real_,
    periods            = period_df,
    loadings           = loadings_df,
    iterations         = cpp_out$iterations,
    n_series           = nVar,
    n_periods          = nperiods,
    n_obs              = nrow(data),
    alpha_F            = cpp_out$alpha_F,
    alpha_B            = cpp_out$alpha_B,
    eigenvalue         = cpp_out$eigenvalue,
    variance_explained      = cpp_out$variance_explained,
    variance_explained_dim2 = cpp_out$variance_explained_dim2,
    call               = cl,
    settings           = list(
      agg_interval    = agg_interval,
      multiple        = multiple,
      n_dim           = n_dim,
      smoothing       = smoothing,
      tol             = tol,
      start_date      = min_date,
      end_date        = max_date,
      fiscal_year_end = fiscal_year_end,
      smooth_seed     = as.integer(smooth_seed)
    )
  )
  class(result) <- "extract"
  result
}

# ============================================================================
# Internal helpers
# ============================================================================

#' Assign a period index to each observation
#' @noRd
.assign_period <- function(year, month, day, date_vec,
                            agg_interval, multiple,
                            min_date, max_date) {
  min_year  <- as.integer(format(min_date, "%Y"))
  min_month <- as.integer(format(min_date, "%m"))
  min_qtr   <- ceiling(min_month / 3)

  switch(agg_interval,
    annual = {
      as.integer((year - min_year) / multiple) + 1L
    },
    multi_year = {
      as.integer((year - min_year) / multiple) + 1L
    },
    quarterly = {
      qtr <- ceiling(month / 3)
      (year - min_year) * 4L - (min_qtr - 1L) + qtr
    },
    monthly = {
      (year - min_year) * 12L - (min_month - 1L) + month
    },
    daily = {
      as.integer(date_vec - min_date) + 1L
    }
  )
}

#' Weighted aggregate of issue values into period × variable matrix
#' Returns 0 (NA sentinel) for missing cells.
#' @noRd
.aggregate_issues <- function(varname, index, n, period, year, month,
                               agg_interval, min_date, multiple = 1L) {
  vars      <- sort(unique(varname))
  nVar      <- length(vars)
  max_period <- max(period, na.rm = TRUE)
  min_period <- min(period, na.rm = TRUE)
  nperiods  <- max_period - min_period + 1L

  mat <- matrix(0.0, nrow = nperiods, ncol = nVar)
  colnames(mat) <- vars

  for (i in seq_along(varname)) {
    p  <- period[i] - min_period + 1L
    v  <- match(varname[i], vars)
    if (is.na(p) || p < 1 || p > nperiods) next
    # weighted cumulation: stored temporarily as (weighted_sum, weight)
    # we use two-pass: collect, then divide
  }

  # two-pass: accumulate weighted sums and counts, then divide
  wsum  <- matrix(0.0, nrow = nperiods, ncol = nVar)
  wcnt  <- matrix(0.0, nrow = nperiods, ncol = nVar)
  for (i in seq_along(varname)) {
    p <- period[i] - min_period + 1L
    v <- match(varname[i], vars)
    if (is.na(p) || p < 1 || p > nperiods || is.na(index[i])) next
    wsum[p, v] <- wsum[p, v] + index[i] * n[i]
    wcnt[p, v] <- wcnt[p, v] + n[i]
  }
  has_data <- wcnt > 0
  mat[has_data] <- wsum[has_data] / wcnt[has_data]  # 0 where missing

  # build period descriptor data frame from unique periods
  # we need one row per period index
  period_df <- .build_period_df(period, year, month, min_period, nperiods,
                                agg_interval, min_date, multiple)

  list(mat = mat, var_names = vars, period_df = period_df)
}

#' Build a data frame describing each period
#' @noRd
.build_period_df <- function(period, year, month, min_period, nperiods,
                              agg_interval, min_date, multiple = 1L) {
  min_year  <- as.integer(format(min_date, "%Y"))
  min_month <- as.integer(format(min_date, "%m"))
  min_qtr   <- ceiling(min_month / 3)

  yr  <- integer(nperiods)
  mo  <- integer(nperiods)

  for (i in seq_len(nperiods)) {
    switch(agg_interval,
      annual = {
        yr[i] <- min_year + (i - 1L) * multiple
        mo[i] <- 0L
      },
      multi_year = {
        yr[i] <- min_year + (i - 1L) * multiple
        mo[i] <- 0L
      },
      quarterly = {
        q_abs <- min_qtr + i - 1L
        yr[i] <- min_year + (q_abs - 1L) %/% 4L
        mo[i] <- ((q_abs - 1L) %% 4L + 1L) * 3L  # last month of quarter
      },
      monthly = {
        m_abs <- min_month + i - 1L
        yr[i] <- min_year + (m_abs - 1L) %/% 12L
        mo[i] <- (m_abs - 1L) %% 12L + 1L
      },
      daily = {
        d <- min_date + (i - 1L)
        yr[i] <- as.integer(format(d, "%Y"))
        mo[i] <- as.integer(format(d, "%m"))
      }
    )
  }

  data.frame(period  = seq_len(nperiods),
             year    = yr,
             month   = mo,
             quarter = ifelse(mo > 0L, ceiling(mo / 3L), 0L),
             stringsAsFactors = FALSE)
}
