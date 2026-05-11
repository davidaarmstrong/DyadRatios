#' Run MCalc non-interactively
#'
#' Constructs the sequence of keystrokes that MCalc expects, writes them to a
#' temporary file, and pipes that file into the MCalc executable via
#' \code{system()}.  The first argument can be either a path to an existing
#' MCalc-format data file \emph{or} an R data frame; in the latter case the
#' data are written to a temporary file in the correct format before being
#' passed to MCalc.
#'
#' @param data Either a character string giving the full path to a
#'   MCalc-format data file, \emph{or} a data frame containing the survey
#'   series.  When a data frame is supplied the columns specified by
#'   \code{varname_col}, \code{date_col}, \code{index_col}, and \code{n_col}
#'   are used to build the input file automatically.
#' @param varname_col Character. Name of the column in \code{data} that holds
#'   the variable/series identifier.  Default \code{"varname"}.  Ignored when
#'   \code{data} is a file path.
#' @param date_col Character. Name of the column in \code{data} that holds the
#'   observation date (a \code{Date} object or a character string in any
#'   unambiguous format recognised by \code{\link[base]{as.Date}}).
#'   Default \code{"date"}.  Ignored when \code{data} is a file path.
#' @param index_col Character. Name of the column that holds the numeric index
#'   value (e.g. percentage agreeing).  Default \code{"index"}.  Ignored when
#'   \code{data} is a file path.
#' @param n_col Character or \code{NULL}. Name of the column that holds the
#'   sample size for each observation.  When \code{NULL} (default) a
#'   placeholder of \code{1000} is used for every row, which leaves relative
#'   weighting uniform.  Ignored when \code{data} is a file path.
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
#'   aggregation, the sub-annual start period (quarter 1–4, month 01–12, or
#'   day 01–31).  Ignored for annual aggregation.  \code{NULL} uses period 1.
#' @param end_year Integer or \code{NULL}. Restrict estimation to periods on
#'   or before this year.  \code{NULL} (default) accepts MCalc's latest
#'   available date.
#' @param end_period Integer or \code{NULL}. Sub-annual end period.
#'   \code{NULL} uses the last period of the year (4, 12, or 31).
#' @param output_root Character or \code{NULL}. Root name for the output
#'   \code{.log} and \code{.csv} files.  When \code{NULL} and \code{data} is a
#'   file path, MCalc uses the input file's stem.  When \code{NULL} and
#'   \code{data} is a data frame, defaults to \code{"mcalc_output"}.
#' @param mcalc_path Character. Full path to the MCalc executable.  Defaults
#'   to \code{"/usr/local/bin/MCalc64"}.
#' @param read_output Logical. If \code{TRUE} (default), reads and returns the
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
#' # --- from a file path ---------------------------------------------------
#' out <- run_mcalc("/Users/me/data/mysurvey.txt",
#'                  mcalc_path = "/Users/me/bin/MCalc64")
#' head(out$data)
#'
#' # --- from a data frame --------------------------------------------------
#' # with default column names (varname, date, index, n)
#' out2 <- run_mcalc(my_df,
#'                   n_dim      = 2,
#'                   smoothing  = FALSE,
#'                   mcalc_path = "/Users/me/bin/MCalc64")
#'
#' # with custom column names
#' out3 <- run_mcalc(survey_df,
#'                   varname_col = "item",
#'                   date_col    = "fielddate",
#'                   index_col   = "pct_agree",
#'                   n_col       = "sample_size",
#'                   start_year  = 1990,
#'                   end_year    = 2020,
#'                   mcalc_path  = "/Users/me/bin/MCalc64")
#' }
#'
#' @export
run_mcalc <- function(data,
                      varname_col  = "varname",
                      date_col     = "date",
                      index_col    = "index",
                      n_col        = NULL,
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

  # --- validate shared arguments ----------------------------------------------
  agg_interval <- match.arg(agg_interval, c("a", "q", "m", "d", "o"))
  n_dim        <- as.integer(n_dim)
  if (!n_dim %in% 1:2) stop("n_dim must be 1 or 2")
  if (!file.exists(mcalc_path))
    stop("MCalc executable not found at: ", mcalc_path)

  # --- resolve input: file path or data frame ---------------------------------
  temp_data_file <- NULL   # will be set (and later cleaned up) for data frames

  if (is.data.frame(data)) {

    # -- validate column names -------------------------------------------------
    required_cols <- c(varname_col, date_col, index_col)
    missing_cols  <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0)
      stop("Column(s) not found in data: ",
           paste(missing_cols, collapse = ", "))
    if (!is.null(n_col) && !n_col %in% names(data))
      stop("n_col '", n_col, "' not found in data")

    # -- coerce and format columns ---------------------------------------------
    varnames <- as.character(data[[varname_col]])
    dates    <- as.Date(data[[date_col]])   # handles Date objects and strings
    if (anyNA(dates))
      stop("date_col contains values that could not be parsed as dates")
    date_str <- format(dates, "%m/%d/%Y")  # MCalc expects MM/DD/YYYY
    index    <- as.numeric(data[[index_col]])
    n_vals   <- if (is.null(n_col)) rep(1000L, nrow(data))
                else as.integer(data[[n_col]])

    # -- write to temp file (same dir so output lands somewhere predictable) ---
    temp_dir       <- tempdir()
    temp_data_file <- file.path(temp_dir, "mcalc_input.txt")
    lines          <- paste(varnames, date_str, index, n_vals)
    writeLines(lines, temp_data_file)

    input_file  <- temp_data_file
    output_dir  <- temp_dir
    # default root name for data-frame input (temp file stem would be ugly)
    if (is.null(output_root)) output_root <- "mcalc_output"

  } else if (is.character(data) && length(data) == 1) {

    input_file  <- normalizePath(data, mustWork = TRUE)
    output_dir  <- dirname(input_file)
    # output_root stays NULL → MCalc uses the input file stem

  } else {
    stop("'data' must be either a data frame or a single file path string")
  }

  # --- build the stdin sequence -----------------------------------------------
  inputs <- character(0)

  # MCalc saves the last-used filename in ~/LastFile.txt and prompts to reuse it
  last_file_path <- path.expand("~/LastFile.txt")
  if (file.exists(last_file_path)) {
    inputs <- c(inputs, "n")        # don't reuse — supply new path below
  }
  inputs <- c(inputs, input_file)   # full path to data file

  inputs <- c(inputs, agg_interval)
  if (agg_interval == "o") inputs <- c(inputs, "2")  # multiple-year factor

  inputs <- c(inputs, "n")   # topic-code selection

  # start date -----------------------------------------------------------------
  if (is.null(start_year)) {
    inputs <- c(inputs, "y")
  } else {
    inputs <- c(inputs, "n", as.character(as.integer(start_year)))
    if (agg_interval == "q") {
      inputs <- c(inputs,
                  as.character(if (is.null(start_period)) 1L
                               else as.integer(start_period)))
    } else if (agg_interval %in% c("m", "d")) {
      inputs <- c(inputs,
                  sprintf("%02d", if (is.null(start_period)) 1L
                                  else as.integer(start_period)))
    }
    if (agg_interval == "d") inputs <- c(inputs, "01")
  }

  # end date -------------------------------------------------------------------
  if (is.null(end_year)) {
    inputs <- c(inputs, "y")
  } else {
    inputs <- c(inputs, "n", as.character(as.integer(end_year)))
    if (agg_interval == "q") {
      inputs <- c(inputs,
                  as.character(if (is.null(end_period)) 4L
                               else as.integer(end_period)))
    } else if (agg_interval %in% c("m", "d")) {
      inputs <- c(inputs,
                  sprintf("%02d", if (is.null(end_period)) 12L
                                  else as.integer(end_period)))
    }
    if (agg_interval == "d") inputs <- c(inputs, "31")
  }

  inputs <- c(inputs, as.character(n_dim))
  inputs <- c(inputs, if (smoothing) "y" else "n")
  inputs <- c(inputs, if (is.null(output_root)) "y" else output_root)

  # --- write stdin sequence and run MCalc -------------------------------------
  stdin_file <- tempfile(fileext = ".txt")
  on.exit({
    unlink(stdin_file)
    if (!is.null(temp_data_file)) unlink(temp_data_file)
  }, add = TRUE)
  writeLines(inputs, stdin_file)

  cmd     <- paste(shQuote(mcalc_path), "<", shQuote(stdin_file))
  console <- system(cmd, intern = TRUE)

  # --- determine output file paths --------------------------------------------
  root     <- if (is.null(output_root))
                tools::file_path_sans_ext(basename(input_file))
              else output_root
  log_path <- file.path(output_dir, paste0(root, ".log"))
  csv_path <- file.path(output_dir, paste0(root, ".csv"))

  result <- list(
    console  = console,
    log_file = log_path,
    csv_file = csv_path
  )

  if (read_output && file.exists(csv_path)) {
    result$data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  }

  result
}
