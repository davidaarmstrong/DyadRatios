#' Extract mood estimates as a data frame
#'
#' Combines the period descriptor table from an \code{\link{extract}} result
#' with the estimated mood trajectory, returning a plain \code{data.frame}
#' that is convenient for further analysis or export.
#'
#' @param obj An object of class \code{"extract"} returned by
#'   \code{\link{extract}}.
#' @param ... Ignored; included for potential future use.
#'
#' @return A \code{data.frame} with one row per time period.  Columns
#'   \code{period}, \code{year}, \code{month}, and \code{quarter} are
#'   inherited from the period table inside \code{obj}.  When a single
#'   dimension was estimated an additional column \code{mood} is appended.
#'   When two dimensions were estimated, columns \code{mood_dim1} and
#'   \code{mood_dim2} are appended instead.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   varname = rep(c("a", "b", "c"), each = 20),
#'   date    = rep(seq(as.Date("1980-01-01"), by = "year", length.out = 20), 3),
#'   index   = 50 + rnorm(60, 0, 5),
#'   n       = 1000L
#' )
#' res <- extract(dat, n_col = "n", smoothing = FALSE)
#' mood_df <- get_mood(res)
#' head(mood_df)
#'
#' @export
get_mood <- function(obj, ...) {
  out <- obj$periods
  if (is.na(obj$mood_dim2[1])) {
    out$mood     <- obj$mood
  } else {
    out$mood_dim1 <- obj$mood
    out$mood_dim2 <- obj$mood_dim2
  }
  out
}
