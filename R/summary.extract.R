summary.extract <-
function(object, ...) {
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
