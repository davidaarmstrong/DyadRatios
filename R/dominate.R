#' calculates mmood - low level function called by extract, not intended to be called by the user. 
#' @noRd
dominate <-
function(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha,csign) {
nitems<- numeric(nperiods)
if (fb==2) alpha1<-alpha
if (fb==1) {
  unexp<-numeric(1)
  everlap<- integer(1)
  alpha<- 1
  alpha1<- 1
  } 
  
  if (fb == 1) {
    startper <- 1
    mood[fb, startper] <- 100
    firstj <- 2
    lastj <- nperiods
    stepj <- 1
    jprev <- 1
  } else {
    startper <- nperiods
    mood[fb, startper] <- mood[1, nperiods] #reuse forward metric
    firstj <- nperiods - 1
    lastj <- 1
    stepj <- -1
    jprev <- nperiods
  } #    end if
for (j in seq(firstj,lastj,by=stepj)) {  
  mood[fb, j] <- 0
  everlap <- 0 ## of years which have contributed sums to mood
  if (fb == 1) {
    firstj2 <- 1
    lastj2 <- j - 1
  } else  {
    firstj2 <- j + 1
    lastj2 <- nperiods
  } # end if

  for (j2 in firstj2:lastj2) { 
    sum <- 0     #has already been estimated
    consum <- 0  #sum of communalities across issues
    overlap <- 0
    for (v in 1:nvar) { 
      xj <- issue[j, v]                      #xj is base year value
      sngx2 <- issue[j2, v]                  #sngx2 is comparison year value
      if (!is.na(xj) && !is.na(sngx2)) {  
        overlap <- overlap + 1               #numb of issues contributing to sum
        ratio <- xj / sngx2
        if (csign[v] < 0)  ratio <- 1 / ratio
        sum <- sum + valid[v] * ratio * mood[fb, j2] 
        consum <- consum + valid[v]
      } #              end if
    } #next v
    if (overlap > 0) {
      everlap <- everlap + 1
      mood[fb, j] <- mood[fb, j] + sum / consum
    } # end if
  } #next j2
  nitems[j] <- everlap
  if (everlap > 0) mood[fb, j] <- mood[fb, j] / everlap else mood[fb, j] <- mood[fb, jprev] #if undefined, set to lag(mood)
  jprev <- j #last value of j, whether lead or lag
} #next j
  if (smoothing == TRUE) {
    alpha<- esmooth(mood, fb, alpha)     #NOW SMOOTH USING ALPHA
    mood.sm<- mood[fb,] #set up alternate vector mood.sm
    for (t in 2:nperiods) { 
      mood.sm[t]<- alpha*mood[fb,t]+(1-alpha)*mood.sm[t-1]
    } #end for
    mood[fb,]<- mood.sm #now assign back smoothed version
  } else {
    alpha1 <- 1
    alpha <- 1
  } 
  if (smoothing == TRUE && fb == 1) alpha1 <- alpha
dominate.out<- list(alpha1=alpha1,alpha=alpha,latent=mood[fb,]) #output object
return(dominate.out)  
#  return(mood[fb,])
}
