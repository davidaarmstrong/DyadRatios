plot.extract <-
function(x, ...) {
  dim<- x$dimensions
  T<- x$T
  vect1<-x$latent1
  t<-seq(1:T)
  if (dim>1) {
    vect2<-x$latent2
    miny<-min(vect1)
    if (miny>min(vect2)) miny<-min(vect2)
    maxy<-max(vect1)
    if (maxy<max(vect2)) maxy<-max(vect2)
    dummy<-rep(miny,T-1) #dummy is a fake variable used to reset axes to handle min/max of both series
    dummy[T]<-maxy
    leg.text<-c("","Dimension 1","Dimension 2")
    plot(t,dummy,type="l",lty=0,main="Final Estimation Results: Two Dimensions",xlab="Time Point",ylab="Latent Variables", ...)
    lines(t,vect1,col=1)
    lines(t,vect2,col=2)
    legend(1,maxy,leg.text,col=c(0,1,2),lty=c(0,1,1))
  } else {
    plot(t,vect1,type="l",main="Final Estimation Results",xlab="Time Point",ylab="Latent Variable", ...)
    if (dim == 2) lines(t,vect2,col=2)
    }
  }
