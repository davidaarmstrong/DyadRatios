#' Find periods for aggregation and estimation.  Low-level function called by extract, not intended to be called by the user. 
#' @noRd
findper <-
function(unit,curdate,mind,miny,minper,aggratio) { #returns intFindPer
  datcurdate<-curdate
  class(datcurdate)<-"Date"
  mo <- month(datcurdate)
  qu <- 1 + as.integer((mo - 1)/3)
  dy <- day(datcurdate)
  yr <- year(datcurdate)
  arinv<- 1/aggratio
  if (unit == "D") intFindPer <- curdate - mind +1 #curdate - mindate + 1
  if (unit == "A" || unit == "O") intFindPer <- as.integer((yr - miny) / aggratio) + 1
  if (unit == "Q") part <- qu
  if (unit == "M") part <- mo
  if (unit == "Q" || unit == "M") intFindPer <- (yr - miny - 1) * arinv + part + (arinv - (minper - 1))
  return(intFindPer)
  }
