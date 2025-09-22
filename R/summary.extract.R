#' Summary Method for extract Objects
#' 
#' Prints a summary from objects estimated with the the extract function. 
#' 
#' @param object An object of class "extract", typically a result of the extract function.
#' @param ... Other arguments to be passed down (currently unused).
#' @return A data frame with variables for the question name, number of cases, loading as well as mean and standard deviation of the series. 
#' @usage NULL
#' @method summary extract
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
#' summary(dr_out)
summary.extract <- function(object, ...) {
  T=object$T
  nvar=object$nvar
  dim<- object$dimensions
  vn<- c(object$varname,"Variable Name")
  vn<- format(vn,justify="right")
  nc<- format(object$N,justify="right")
  ld<- format(object$loadings1,digits=3,justify="right")
  mean<- format(object$means,digits=6,justify="right")
  sd<- format(object$std.deviations,digits=6,justify="right")
  cat("Variable Loadings and Descriptive Information: Dimension 1\n")
  cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
  for (v in 1:nvar) {
    cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
    }
  if (dim == 2) {
    ld<- format(object$loadings2,digits=3,justify="right")
    cat("\nVariable Loadings and Descriptive Information: Dimension 2\n")
    cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
    for (v in 1:nvar) {
      cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
    }
  }
}
