aggregate <-
function(varname,date,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) {   #
    #READ A NEW RECORD, CALCULATE PERIOD, AND SET UP AGGREGATION INTO MAT.ISSUE[NPERIODS,NVAR] 
  vl<- character(nvar)
  mind<- as.integer(mindate)/86400
  maxd<- as.integer(maxdate)/86400
  vfac<- factor(varname) #make a factor vector
  vlev<- levels(vfac)    #find unique categories
  Mat.Issue<- array(dim=c(nperiods,nvar))

  nrec<-length(varname) #added for R compatibility
  lp<- 0
  per<- 0
  x<- 0
  c<- 0
  nkeep<- 0
  lv<- "0"
  for (record in 1:nrec) { # MASTER LOOP THROUGH INPUT DATA, 1 TO NREC
    if (ncases[record] == 0 || is.na(ncases[record])) ncases[record] <- 1000
    mo <- findmonth(date[record])
    qu <- 1 + as.integer((mo - 1)/3)
    dy <- findday(date[record])
    yr <- findyear(date[record])
    curdate<- as.integer(date[record])
    if (curdate >= mind &&  curdate <= maxd) {  #is date within range?
    nkeep <- nkeep + 1
    if (nkeep==1) { #startup routine for first good case
      firstcase<- TRUE
      lp <- findper(unit,curdate,mind,miny,minper,aggratio)
      lv <- varname[record]
      x <- index[record] * ncases[record] #start new sums for case 1
      c <- ncases[record]
      for (i in 1:nvar) {
        if (lv==vlev[i]) v=i #determine v by matching to position of labels vector
        } #end for
      } else {
      firstcase<- FALSE
      } #end if
    if (firstcase == FALSE) { #skip over the rest for first good case
    per<- findper(unit,curdate,mind,miny,minper,aggratio) #here we translate date into agg category
    if ((varname[record] !=  lv) || (per !=lp)) { #found a new period or variable name
      if (lp > 0 &&  lp <= nperiods) {
        Mat.Issue[lp, v] <- x / c #recompute for either period or var change
        x<- 0
        c<- 0
        }
      if (varname[record] !=  lv) { #new var only
        for (i in 1:nvar) {
          if (varname[record]==vlev[i]) v=i #determine v by matching to position of labels vector
          } #end for
        vl[v]<- varname[record] #this will only catch names that have good cases
        lv<-vl[v]  #reassign new varname to lastvar
      } # new var
      lp <- findper(unit,curdate,mind,miny,minper,aggratio)
      x <- index[record] * ncases[record] #start new sums for current case
      c <- ncases[record]
    } else {
      x<- x + index[record] * ncases[record] #a continuing case, increment sums
      c<- c + ncases[record]
    }
    } # end of first case special loop
  } #end of date test loop
  } #newrec: next record
  vl<- vlev #overwrite previous assignment which had good names only
  agglist<- list(lab=vl,iss=Mat.Issue)
  return(agglist) #list includes labels and issue matrix
  }
