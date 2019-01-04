findyear <-
function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findyear<-as.integer(v[6])+1900
  }
