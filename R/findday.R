findday <-
function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findday<-as.integer(v[4])
  }
