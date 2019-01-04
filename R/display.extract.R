display <- function(out,filename=NULL) {
  UseMethod("display")
}

display.extract <-
function(out,filename=NULL) {
   if (is.null(filename)) filename=""
   d<-out$dimensions
   p<-out$period
   m<-out$latent1
   if (d==2) m2<-out$latent2     
   T<-out$T
   mo=100*(p-as.integer(p))
   for (t in 1:T) {
     yr<-format(as.integer(p[t]),nsmall=0)
     month<-format(mo[t],digits=2)
     lat1<-format(m[t],nsmall=3)
     if (d==1) {
       cat(c(yr,month,lat1),fill=TRUE,file=filename,append=TRUE)
     } else {
       lat2<-format(m2[t],nsmall=3)
       cat(c(yr,month,lat1,lat2),fill=TRUE,file=filename,append=TRUE)
     }
   }
 }
