findmonth <-
function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findmonth<-as.integer(v[5])+1
  }
