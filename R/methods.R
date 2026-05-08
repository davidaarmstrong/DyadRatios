#' Bootstrap the Dyad Ratios estimate
#'
#' Generates a sampling distribution around the latent mood trajectory by
#' repeatedly drawing synthetic survey marginals from a binomial model and
#' re-running \code{\link{extract}}.  The original (unperturbed) estimate is
#' used as the point estimate; the bootstrap draws characterise uncertainty
#' around it.
#'
#' All model parameters (aggregation interval, column names, smoothing, etc.)
#' are taken directly from the stored call inside \code{obj}, so there is no
#' risk of the bootstrap replications being run with different settings than
#' the original.
#'
#' @param obj   An object of class \code{"extract"} returned by
#'   \code{\link{extract}}.  The stored \code{call} is used to replay the
#'   estimation identically on each bootstrap draw.
#' @param data  The original data frame that was passed to \code{\link{extract}}
#'   to produce \code{obj}.  The index column is perturbed for each replication;
#'   all other columns are used as-is.
#' @param R         Integer. Number of bootstrap replications. Default \code{200}.
#' @param level     Numeric in (0, 1). Confidence level for the interval.
#'   Default \code{0.95}.
#' @param pw        Logical. Whether to calculate pairwise differences between times
#' @param seed      Integer or \code{NULL}. Passed to \code{set.seed} before
#'   the bootstrap loop. Default \code{NULL}.
#' @param parallel  Logical. Parallelise the outer loop using \pkg{parallel} if
#'   available? Useful for large \code{R}. Default \code{FALSE}.
#'
#' @return A list of class \code{"boot_dr"} with two or four components
#'   depending on whether the original \code{obj} used one or two dimensions:
#'   \describe{
#'     \item{\code{estimates}}{A \code{data.frame} with one row per time period
#'       and columns \code{period}, \code{year}, \code{month}, \code{quarter},
#'       \code{mood} (original point estimate), \code{lower}, and \code{upper}
#'       (confidence bounds at \code{(1-level)/2} and \code{1-(1-level)/2}).
#'       When \code{n_dim = 2}, three additional columns are appended:
#'       \code{mood_dim2}, \code{lower_dim2}, and \code{upper_dim2}.}
#'     \item{\code{samples}}{An \code{n_periods} × \code{R} matrix of raw
#'       bootstrap dimension-1 trajectories.}
#'     \item{\code{samples_dim2}}{(\code{n_dim = 2} only) An \code{n_periods}
#'       × \code{R} matrix of raw bootstrap dimension-2 trajectories.}
#'   }
#'   The list also carries attributes \code{R} (number of successful
#'   replications), \code{level}, \code{agg_interval}, and \code{n_dim}.
#'
#' @details
#' Each replication draws \eqn{y_i \sim \text{Binomial}(n_i, p_i)} where
#' \eqn{p_i} is the observed proportion and \eqn{n_i} is the sample size, then
#' replaces the index column with \eqn{y_i / n_i} (rescaled to the same 0–100
#' vs 0–1 convention as the original) and re-runs \code{\link{extract}} using
#' the exact call stored in \code{obj}.  Replications that error (e.g. due to
#' a degenerate draw) are silently discarded; \code{attr(result, "R")} reports
#' how many succeeded.
#'
#' @seealso \code{\link{extract}}, \code{\link{plot.boot_dr}}
#'
#' @examples
#' # Build a small synthetic dataset: 4 items measured annually over 20 years
#' set.seed(42)
#' n_years <- 20
#' years   <- seq(1980, length.out = n_years)
#' items   <- c("item_a", "item_b", "item_c", "item_d")
#'
#' dat <- do.call(rbind, lapply(items, function(item) {
#'   data.frame(
#'     varname = item,
#'     date    = as.Date(paste0(years, "-07-01")),
#'     index   = 50 + cumsum(rnorm(n_years, 0, 1.5)) + rnorm(n_years, 0, 2),
#'     n       = sample(800:1200, n_years, replace = TRUE)
#'   )
#' }))
#'
#' # Run the original estimate first
#' res <- extract(dat, n_col = "n", smoothing = FALSE)
#'
#' # Bootstrap with 100 replications (use more in practice)
#' boot <- boot_dr(res, dat, R = 100, seed = 1)
#'
#' # estimates is the summary data frame
#' head(boot$estimates)
#'
#' # mood is the original point estimate; lower/upper are the 95% CI
#' boot$estimates[, c("year", "mood", "lower", "upper")]
#'
#' # samples is the raw n_periods x R matrix of bootstrap trajectories
#' dim(boot$samples)
#'
#' # Plot the trajectory with uncertainty ribbon
#' plot(boot)
#'
#' @importFrom utils combn
#' @export
boot_dr <- function(obj, data, R = 200L,
                    level = 0.95,
                    pw = FALSE,
                    seed = NULL, parallel = FALSE) {

  stopifnot(inherits(obj, "extract"))
  stopifnot(is.data.frame(data))
  stopifnot(level > 0, level < 1)
  if (!is.null(seed)) set.seed(seed)

  # ---- recover column names and settings from the stored call ---------------
  call_list <- as.list(obj$call)
  index_col <- if (!is.null(call_list$index_col)) as.character(call_list$index_col) else "index"
  n_col     <- if (!is.null(call_list$n_col))     as.character(call_list$n_col)     else NULL
  n_dim     <- obj$settings$n_dim   # 1 or 2

  if (!index_col %in% names(data))
    stop("Column '", index_col, "' not found in data.")
  if (!is.null(n_col) && !n_col %in% names(data))
    stop("Column '", n_col, "' not found in data.")

  # ---- argument list for replaying the call ---------------------------------
  # as.list(call)[-1] drops the function name; we replace $data each iteration
  boot_args <- as.list(obj$call)[-1]

  # ---- prepare binomial draw parameters ------------------------------------
  idx <- data[[index_col]]
  ns  <- if (!is.null(n_col)) as.numeric(data[[n_col]]) else rep(1000, nrow(data))

  pct_scale <- max(idx, na.rm = TRUE) > 1
  prop <- if (pct_scale) idx / 100 else idx
  prop <- pmin(pmax(prop, 0), 1)

  one_rep <- function(r) {
    sim_y          <- rbinom(length(prop), size = round(ns), prob = prop)
    sim_index      <- sim_y / ns
    if (pct_scale) sim_index <- sim_index * 100

    d              <- data
    d[[index_col]] <- sim_index

    the_args       <- boot_args
    the_args$data  <- d

    res <- tryCatch(do.call(extract, the_args), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    if (n_dim == 2L)
      list(mood = res$mood, mood_dim2 = res$mood_dim2)
    else
      res$mood
  }

  # ---- run replications -----------------------------------------------------
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    nc <- max(1L, parallel::detectCores() - 1L)
    cl <- parallel::makeCluster(nc)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl,
      c("data", "prop", "ns", "index_col", "pct_scale", "boot_args", "n_dim"),
      envir = environment())
    parallel::clusterEvalQ(cl, library(DyadRatios))
    results <- parallel::parLapply(cl, seq_len(R), one_rep)
  } else {
    results <- lapply(seq_len(R), one_rep)
  }

  ok      <- !sapply(results, is.null)
  results <- results[ok]
  if (length(results) == 0L) stop("All bootstrap replications failed.")

  # ---- build sample matrices ------------------------------------------------
  alpha <- (1 - level) / 2
  if (n_dim == 2L) {
    mood_mat      <- do.call(cbind, lapply(results, `[[`, "mood"))
    mood_mat_dim2 <- do.call(cbind, lapply(results, `[[`, "mood_dim2"))
  } else {
    mood_mat <- do.call(cbind, results)
  }

  # ---- assemble summary data frame ------------------------------------------
  estimates <- cbind(
    obj$periods,
    data.frame(
      mood  = obj$mood,
      lower = apply(mood_mat, 1, quantile, probs = alpha,     na.rm = TRUE),
      upper = apply(mood_mat, 1, quantile, probs = 1 - alpha, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  )
  if (n_dim == 2L) {
    estimates$mood_dim2  <- obj$mood_dim2
    estimates$lower_dim2 <- apply(mood_mat_dim2, 1, quantile, probs = alpha,     na.rm = TRUE)
    estimates$upper_dim2 <- apply(mood_mat_dim2, 1, quantile, probs = 1 - alpha, na.rm = TRUE)
  }

  # ---- assemble return list -------------------------------------------------
  out <- list(estimates = estimates, samples = mood_mat)
  if (n_dim == 2L) out$samples_dim2 <- mood_mat_dim2

  # ---- calculate optional pairwise differences ------------------------------
  if (pw) {
    period <- estimates$period
    combs  <- combn(nrow(mood_mat), 2)
    D      <- matrix(0, nrow = nrow(mood_mat), ncol = ncol(combs))
    D[cbind(combs[1, ], seq_len(ncol(combs)))] <- -1
    D[cbind(combs[2, ], seq_len(ncol(combs)))] <-  1
    diffs  <- t(mood_mat) %*% D
    p_diff <- apply(diffs, 2, function(x) mean(x > 0))
    p_diff <- ifelse(p_diff < 0.5, 1 - p_diff, p_diff)
    diff   <- matrix(estimates$mood, nrow = 1) %*% D
    out$pw <- data.frame(
      p1     = period[combs[1, ]],
      p2     = period[combs[2, ]],
      diff   = c(diff),
      p_diff = p_diff
    )
  }

  attr(out, "R")            <- sum(ok)
  attr(out, "level")        <- level
  attr(out, "agg_interval") <- obj$settings$agg_interval
  attr(out, "n_dim")        <- n_dim
  class(out) <- "boot_dr"
  out
}


#' Plot a boot_dr object
#'
#' Draws a ribbon plot of the bootstrap confidence interval around the
#' original mood estimate.  Requires \pkg{ggplot2}; falls back to base R if
#' unavailable.
#'
#' @param x     A \code{boot_dr} object returned by \code{\link{boot_dr}}.
#' @param dim   Integer.  Which dimension to plot (\code{1} or \code{2}).
#'   Dimension 2 is only available when the original \code{\link{extract}}
#'   call used \code{n_dim = 2}.  Default \code{1}.
#' @param title Character. Plot title.
#' @param ylab  Character. Y-axis label.
#' @param ...   Ignored.
#' @return Invisibly, the ggplot2 object or \code{NULL} (base R).
#' @importFrom graphics polygon lines
#' @importFrom grDevices adjustcolor
#' @export
#' @method plot boot_dr
plot.boot_dr <- function(x, dim = 1L,
                         title = "Estimated Public Mood",
                         ylab = "Mood", ...) {
  est <- x$estimates
  n_dim_boot <- attr(x, "n_dim")

  if (dim == 2L) {
    if (is.null(n_dim_boot) || n_dim_boot < 2L ||
        !all(c("mood_dim2", "lower_dim2", "upper_dim2") %in% names(est)))
      stop("Dimension 2 results are not available in this boot_dr object.")
    mood_col  <- "mood_dim2"
    lower_col <- "lower_dim2"
    upper_col <- "upper_dim2"
  } else {
    mood_col  <- "mood"
    lower_col <- "lower"
    upper_col <- "upper"
  }

  date_vec <- as.Date(paste(est$year,
                            ifelse(est$month > 0, est$month, 1),
                            "01", sep = "-"))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- data.frame(date  = date_vec,
                     mood  = est[[mood_col]],
                     lower = est[[lower_col]],
                     upper = est[[upper_col]])
    level_pct <- round(attr(x, "level") * 100)
    dim_label <- if (dim == 2L) "  (dimension 2)" else ""
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                           fill = "#2c7bb6", alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(y = mood),
                         colour = "#2c7bb6", linewidth = 0.8) +
      ggplot2::labs(title    = paste0(title, dim_label),
                    subtitle = sprintf("%d%% bootstrap confidence interval  (R = %d)",
                                       level_pct, attr(x, "R")),
                    x = "Year", y = ylab) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    print(p)
    return(invisible(p))
  }

  # Base R fallback
  plot(date_vec, est[[mood_col]], type = "l", col = "#2c7bb6",
       ylim = range(c(est[[lower_col]], est[[upper_col]]), na.rm = TRUE),
       main = title, xlab = "Year", ylab = ylab, ...)
  polygon(c(date_vec, rev(date_vec)),
          c(est[[lower_col]], rev(est[[upper_col]])),
          col = adjustcolor("#2c7bb6", alpha.f = 0.2), border = NA)
  lines(date_vec, est[[mood_col]], col = "#2c7bb6")
  invisible(NULL)
}


#' Print method for extract objects
#'
#' @param x A \code{extract} object returned by \code{\link{extract}}.
#' @param ... Ignored.
#' @export
print.extract <- function(x, ...) {
  cat("\n=== Dyad Ratios Estimation ===\n\n")
  s <- x$settings
  cat(sprintf("Aggregation:   %s\n", s$agg_interval))
  cat(sprintf("Period:        %s to %s\n",
              format(s$start_date, "%Y-%m-%d"),
              format(s$end_date,   "%Y-%m-%d")))
  cat(sprintf("Time points:   %d\n", x$n_periods))
  cat(sprintf("Series:        %d\n", x$n_series))
  cat(sprintf("Observations:  %d\n", x$n_obs))
  cat(sprintf("Dimensions:    %d\n", s$n_dim))
  cat(sprintf("Smoothing:     %s\n", if (s$smoothing) "on" else "off"))
  if (s$smoothing) {
    cat(sprintf("  Alpha (F):   %.3f\n", x$alpha_F))
    cat(sprintf("  Alpha (B):   %.3f\n", x$alpha_B))
  }
  cat(sprintf("Iterations:    %d\n",
              max(x$iterations$iter, na.rm = TRUE)))
  cat(sprintf("Eigenvalue:    %.3f\n", x$eigenvalue))
  cat(sprintf("Var. Explained:%.1f%%\n\n",
              100 * x$variance_explained))

  cat("Mood (first 10 periods):\n")
  idx <- seq_len(min(10, x$n_periods))
  pd  <- x$periods[idx, , drop = FALSE]
  agg <- x$settings$agg_interval
  labels <- if (agg %in% c("quarterly")) {
    paste0(pd$year, " Q", pd$quarter)
  } else if (agg == "monthly") {
    paste0(pd$year, "-", sprintf("%02d", pd$month))
  } else if (agg == "daily") {
    paste0(pd$year, "-", sprintf("%02d", pd$month))
  } else {
    as.character(pd$year)
  }
  vals <- round(x$mood[idx], 2)
  cat(paste(sprintf("  %-10s %.2f", labels, vals), collapse = "\n"))
  if (x$n_periods > 10) cat(sprintf("\n  ... (%d more periods)", x$n_periods - 10))
  cat("\n\n")
  invisible(x)
}


#' Summary method for extract objects
#'
#' Prints a detailed report similar to the log file produced by the original
#' 'MCalc' program, including the iteration history, variable loadings, and
#' variance accounting.
#'
#' @param object A \code{extract} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{object}.
#' @method summary extract
#' @export
summary.extract <- function(object, ...) {
  x <- object
  s <- x$settings
  cat("\n================================================================\n")
  cat("Dyad Ratios Estimation Report\n")
  cat("================================================================\n\n")
  cat(sprintf("Aggregation interval: %s\n", s$agg_interval))
  cat(sprintf("Period: %s to %s\n",
              format(s$start_date, "%Y-%m-%d"),
              format(s$end_date,   "%Y-%m-%d")))
  cat(sprintf("Time points:  %d\n", x$n_periods))
  cat(sprintf("Series used:  %d\n", x$n_series))
  cat(sprintf("Total observations: %d\n", x$n_obs))
  cat(sprintf("Dimensions extracted: %d\n", s$n_dim))
  cat(sprintf("Exponential smoothing: %s\n",
              if (s$smoothing) "on" else "off"))
  cat(sprintf("Convergence tolerance: %.4f\n\n", s$tol))

  cat("----------------------------------------------------------------\n")
  cat("Iteration History\n")
  cat("----------------------------------------------------------------\n")
  cat(sprintf("%-5s  %-11s  %-8s  %-11s  %-8s  %-8s\n",
              "Iter", "Converge", "Crit", "Reliability", "Alpha_F", "Alpha_B"))
  cat(strrep("-", 60), "\n")
  for (i in seq_len(nrow(x$iterations))) {
    r <- x$iterations[i, ]
    cat(sprintf("%5d  %11.5f  %8.3f  %11.3f  %8.3f  %8.3f\n",
                r$iter, r$converge, r$criterion,
                r$reliability, r$alpha_F, r$alpha_B))
  }

  cat("\n----------------------------------------------------------------\n")
  cat("Variable Loadings\n")
  cat("----------------------------------------------------------------\n")
  lo <- x$loadings
  has_dim2 <- s$n_dim == 2 && "dim2" %in% names(lo)
  hdr <- sprintf("%-20s  %5s  %8s  %8s  %8s",
                 "Variable", "N", "Mean", "Std", "Dim1")
  if (has_dim2) hdr <- paste0(hdr, "     Dim2")
  cat(hdr, "\n", strrep("-", if (has_dim2) 66 else 55), "\n", sep = "")
  for (i in seq_len(nrow(lo))) {
    ln <- sprintf("%-20s  %5d  %8.3f  %8.3f  %8.3f",
                  lo$variable[i], lo$n_obs[i],
                  lo$mean[i], lo$std_dev[i], lo$dim1[i])
    if (has_dim2) ln <- paste0(ln, sprintf("  %8.3f", lo$dim2[i]))
    cat(ln, "\n")
  }

  cat("\n----------------------------------------------------------------\n")
  cat("Variance Accounting\n")
  cat("----------------------------------------------------------------\n")
  cat(sprintf("Eigenvalue estimate:    %.3f\n", x$eigenvalue))
  cat(sprintf("Variance explained:     %.1f%%\n\n",
              100 * x$variance_explained))

  invisible(x)
}

#' Plot estimated public mood
#'
#' Produces a time-series plot of the estimated latent mood.  Requires
#' \pkg{ggplot2}.  If \pkg{ggplot2} is not installed, falls back to base R
#' graphics.
#'
#' @param x A \code{extract} object.
#' @param dim Integer.  Which dimension to plot (1 or 2).  Default \code{1}.
#' @param title Character.  Plot title.  Default \code{"Estimated Public Mood"}.
#' @param ylab Character.  Y-axis label.  Default \code{"Mood"}.
#' @param ... Additional arguments passed to the plotting function.
#' @return Invisibly, the plot object (ggplot2) or \code{NULL} (base R).
#' @importFrom graphics abline plot
#' @importFrom stats sd
#' @export
#' @method plot extract
plot.extract <- function(x, dim = 1L, title = "Estimated Public Mood",
                    ylab = "Mood", ...) {
  stopifnot(inherits(x, "extract"))
  mood_vec <- if (dim == 2 && x$settings$n_dim == 2) x$mood_dim2 else x$mood
  pd <- x$periods

  # Build a date approximation from the period table
  date_vec <- as.Date(paste(pd$year,
                            ifelse(pd$month > 0, pd$month, 1),
                            "01", sep = "-"))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- data.frame(date = date_vec, mood = mood_vec)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = mood)) +
      ggplot2::geom_line(colour = "#2c7bb6", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = mean(mood_vec, na.rm = TRUE),
                          linetype = "dashed", colour = "grey50") +
      ggplot2::labs(title = title,
                    x = "Year",
                    y = ylab) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    print(p)
    return(invisible(p))
  } else {
    plot(date_vec, mood_vec, type = "l", col = "#2c7bb6",
         main = title, xlab = "Year", ylab = ylab, ...)
    abline(h = mean(mood_vec, na.rm = TRUE), lty = 2, col = "grey50")
    return(invisible(NULL))
  }
}

