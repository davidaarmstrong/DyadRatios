#' Run MCalc non-interactively
#'
#' Constructs the sequence of keystrokes that MCalc expects, writes them to a
#' temporary file, and pipes that file into the MCalc executable via
#' \code{system()}.  Output \code{.log} and \code{.csv} files are written to
#' the same directory as the input file (MCalc's fixed behaviour).
#'
#' @param file Character. Full path to the MCalc input data file.
#' @param agg_interval Character. Aggregation interval: \code{"a"} (annual,
#'   default), \code{"q"} (quarterly), \code{"m"} (monthly), \code{"d"}
#'   (daily), or \code{"o"} (multiple years).
#' @param n_dim Integer. Number of dimensions to extract: \code{1} (default)
#'   or \code{2}.
#' @param smoothing Logical. \code{TRUE} (default) turns exponential smoothing
#'   on; \code{FALSE} turns it off.
#' @param start_year Integer or \code{NULL}. Restrict estimation to periods
#'   on or after this year.  \code{NULL} (default) accepts MCalc's earliest
#'   available date.
#' @param start_period Integer or \code{NULL}. For quarterly/monthly/daily
#'   aggregation, the sub-annual start period (quarter 1-4, month 1-12, or
#'   day 1-31).  Ignored for annual aggregation.  \code{NULL} uses period 1.
#' @param end_year Integer or \code{NULL}. Restrict estimation to periods on
#'   or before this year.  \code{NULL} (default) accepts MCalc's latest
#'   available date.
#' @param end_period Integer or \code{NULL}. Sub-annual end period.  \code{NULL}
#'   uses the last period of the year (4, 12, or 31 depending on interval).
#' @param output_root Character or \code{NULL}.  Root name for the output
#'   \code{.log} and \code{.csv} files.  \code{NULL} (default) accepts MCalc's
#'   default, which is the stem of the input file name.
#' @param mcalc_path Character. Full path to the MCalc executable.  Defaults
#'   to \code{"/usr/local/bin/MCalc64"}.
#' @param read_output Logical.  If \code{TRUE} (default), reads and returns the
#'   \code{.csv} output as a data frame.  If \code{FALSE}, returns only the
#'   file paths and console output.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{\code{console}}{Character vector of lines printed to stdout.}
#'     \item{\code{log_file}}{Path to the \code{.log} output file.}
#'     \item{\code{csv_file}}{Path to the \code{.csv} output file.}
#'     \item{\code{data}}{Data frame read from the \code{.csv} file (only
#'       present when \code{read_output = TRUE} and the file exists).}
#'   }
#'
#' @examples
#' \dontrun{
#' # 1-dimension run, annual, smoothing on
#' out <- run_mcalc("/Users/me/data/mysurvey.txt")
#' head(out$data)
#'
#' # 2-dimension run, restrict dates, smoothing off
#' out2 <- run_mcalc("/Users/me/data/mysurvey.txt",
#'                   n_dim      = 2,
#'                   smoothing  = FALSE,
#'                   start_year = 1980,
#'                   end_year   = 2020)
#' }
#'
#' @export
run_mcalc <- function(file,
                      agg_interval = "a",
                      n_dim        = 1L,
                      smoothing    = TRUE,
                      start_year   = NULL,
                      start_period = NULL,
                      end_year     = NULL,
                      end_period   = NULL,
                      output_root  = NULL,
                      mcalc_path   = "/usr/local/bin/MCalc64",
                      read_output  = TRUE) {

  # --- validate ---------------------------------------------------------------
  file       <- normalizePath(file, mustWork = TRUE)
  agg_interval <- match.arg(agg_interval, c("a", "q", "m", "d", "o"))
  n_dim      <- as.integer(n_dim)
  if (!n_dim %in% 1:2) stop("n_dim must be 1 or 2")
  if (!file.exists(mcalc_path))
    stop("MCalc executable not found at: ", mcalc_path)

  # --- build the input sequence -----------------------------------------------
  inputs <- character(0)

  # MCalc saves the last-used filename in ~/LastFile.txt and prompts to reuse it
  last_file_path <- path.expand("~/LastFile.txt")
  if (file.exists(last_file_path)) {
    inputs <- c(inputs, "n")          # don't reuse — supply new path below
  }
  inputs <- c(inputs, file)           # full path to data file

  # aggregation interval
  inputs <- c(inputs, agg_interval)

  # multiple-years multiplier (only relevant for "o")
  # (not commonly used; left as a reasonable default of 2)
  if (agg_interval == "o") inputs <- c(inputs, "2")

  # topic-code selection
  inputs <- c(inputs, "n")

  # start date ----------------------------------------------------------------
  if (is.null(start_year)) {
    inputs <- c(inputs, "y")          # accept earliest date
  } else {
    inputs <- c(inputs, "n")          # enter a custom start
    inputs <- c(inputs, as.character(as.integer(start_year)))
    if (agg_interval == "q") {
      inputs <- c(inputs, as.character(if (is.null(start_period)) 1L
                                       else as.integer(start_period)))
    } else if (agg_interval %in% c("m", "d")) {
      inputs <- c(inputs, sprintf("%02d", if (is.null(start_period)) 1L
                                          else as.integer(start_period)))
    }
    if (agg_interval == "d") {
      inputs <- c(inputs, "01")       # day — default to 1st of the month
    }
  }

  # end date ------------------------------------------------------------------
  if (is.null(end_year)) {
    inputs <- c(inputs, "y")          # accept latest date
  } else {
    inputs <- c(inputs, "n")          # enter a custom end
    inputs <- c(inputs, as.character(as.integer(end_year)))
    if (agg_interval == "q") {
      inputs <- c(inputs, as.character(if (is.null(end_period)) 4L
                                       else as.integer(end_period)))
    } else if (agg_interval %in% c("m", "d")) {
      inputs <- c(inputs, sprintf("%02d", if (is.null(end_period)) 12L
                                          else as.integer(end_period)))
    }
    if (agg_interval == "d") {
      inputs <- c(inputs, "31")       # day — default to 31st
    }
  }

  # number of dimensions, smoothing, output root
  inputs <- c(inputs, as.character(n_dim))
  inputs <- c(inputs, if (smoothing) "y" else "n")
  inputs <- c(inputs, if (is.null(output_root)) "y" else output_root)

  # --- write inputs to a temp file and run ------------------------------------
  input_file <- tempfile(fileext = ".txt")
  on.exit(unlink(input_file), add = TRUE)
  writeLines(inputs, input_file)

  cmd     <- paste(shQuote(mcalc_path), "<", shQuote(input_file))
  console <- system(cmd, intern = TRUE)

  # --- determine output file paths --------------------------------------------
  file_dir  <- dirname(file)
  file_stem <- tools::file_path_sans_ext(basename(file))
  root      <- if (is.null(output_root)) file_stem else output_root

  log_path <- file.path(file_dir, paste0(root, ".log"))
  csv_path <- file.path(file_dir, paste0(root, ".csv"))

  result <- list(
    console  = console,
    log_file = log_path,
    csv_file = csv_path
  )

  # --- optionally read the csv ------------------------------------------------
  if (read_output && file.exists(csv_path)) {
    result$data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  }

  result
}
