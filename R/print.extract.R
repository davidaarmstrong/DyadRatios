#' Print method for extract function
#' 
#' @param x An object of class "extract", typically a result of the extract function.
#' @param ... Other arguments to be pased down
#' @return A printed summary of the output from the extract function and the input object returned invisbly. 
#' @usage NULL
#' 
#' @method print extract
#' @export
#' @examples
#' data(jennings)
#' dr_out <- extract(varname = jennings$variable, 
#'                   date = jennings$date, 
#'                   index = jennings$value, 
#'                   ncases = jennings$n, 
#'                   begindt = min(jennings$date), 
#'                   enddt = max(jennings$date), 
#'                   npass=1)
#' print(dr_out)
print.extract <- function(x, ...) {
  cat("\nEstimation report:\n")
  cat("Period:", min(x$period), " to", max(x$period), ", ", x$T, " time points\n")
  cat("Number of series: ", x$nvar + ifelse(is.null(x$dropped), 0, nrow(x$dropped)), "\n")
  cat("Number of usable series: ", x$nvar, "\n")
  cat("Exponential smoothing: ", ifelse(x$smoothing, "Yes", "No"), "\n\n")
  cat("Iteration history: \n")
  cat(" \n")
  print(x$hist, row.names = FALSE, digits=3)
  cat(" \n")
  cat("Total Variance to be Explained = ", sprintf("%.2f", x$totalvar), "\n\n")
  cat("Percent Variance Explained: \n")
  print(x$var_exp, row.names=FALSE, digits=3)
  cat(" \n")
  cat("Final Weighted Average Metric: ", "Mean: ", sprintf("%.2f", x$wtdmean), 
      " St. Dev: ", sprintf("%.2f", x$wtdstd), "\n")
  invisible(x)
}
