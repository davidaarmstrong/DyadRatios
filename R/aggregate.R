#' Create matrix of issue data aggregated by period. This is a low-level function called by extract(), not intended to be called directly by the user
#' @noRd
aggregate <-
function(varname,date,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) {   #
    #READ A NEW RECORD, CALCULATE PERIOD, AND SET UP AGGREGATION INTO MAT.ISSUE[NPERIODS,NVAR] 
  vl<- character(nvar)
  mind<- as.integer(mindate)
  maxd<- as.integer(maxdate)
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
    mo <- month(date[record])
    qu <- 1 + as.integer((mo - 1)/3)
    dy <- day(date[record])
    yr <- year(date[record])
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

# 
# mindt <- min(mindate)
# maxdt <- max(maxdate)
# minyr <- lubridate::year(mindt)
# maxyr <- lubridate::year(maxdt)
# minmo <- lubridate::month(mindt)
# maxmo <- lubridate::month(maxdt)
# minday <- lubridate::day(mindt)
# maxday <- lubridate::day(maxdt)
# minq <- lubridate::quarter(mindt)
# maxq <- lubridate::quarter(maxdt)
#   
# if(unit == "A"){
#   periods <- seq(minyr, maxyr, by=1)
#   obs_periods <- lubridate::year(date)
# }
# if(unit == "Q"){
#   stq <- lubridate::yq(paste0(minyr,"-",minq))
#   endq <- lubridate::yq(paste0(maxyr,"-",maxq))
#   periods <- seq(stq, endq, by="quarter")
#   obs_periods <- lubridate::yq(paste0(lubridate::year(date),"-",lubridate::quarter(date)))
# }
# if(unit == "M"){
#   stm <- lubridate::ym(paste0(minyr,"-",minmo))
#   endm <- lubridate::ym(paste0(maxyr,"-",maxmo))
#   periods <- seq(stm, endm, by="month")
#   obs_periods <- lubridate::ym(paste0(lubridate::year(date),"-",lubridate::month(date)))
# }
# if(unit == "D"){
#   periods <- seq(mindt, maxdt, by="day")
#   obs_periods <- date
# }
# if(unit == "O"){
#   year_periods <- seq(lubridate::ymd(paste0(minyr, "-01-01")), 
#                       lubridate::ymd(paste0(maxyr, "-01-01")), 
#                       by=paste0(mult, " year"))
#   periods <- lubridate::year(year_periods)
#   obs_years <- lubridate::year(date)
#   if(any(obs_years > max(periods)))periods <- c(periods, max(periods) + mult)
#   nper <- length(periods)
#   obs_period <- rep(NA, length(date))
#   o <- outer(date, periods, \(x,y){
#     lubridate::year(x) >= y & lubridate::year(x) < (y + mult)
#   })
#   obs_period <- periods[apply(o, 1, which)]
# }
# 
# index <- input$value
# if(any(index > 1)) index <- index/100
# 
# all_dat <- data.frame(period = periods)
# ag_dat <- data.frame(period = obs_period, 
#                      var = varname, 
#                      index = index, 
#                      ncases = ncases)
# 
# ag_dat <- ag_dat |> 
#   dplyr::mutate(n_pos = index * ncases) |> 
#   dplyr::group_by(period, var) |> 
#   dplyr::summarise(n_pos = sum(n_pos), 
#             ncases = sum(ncases)) |> 
#   dplyr::mutate(pct = n_pos / ncases) |> 
#   dplyr::select(period, var, pct) |> 
#   tidyr::pivot_wider(names_from = var, values_from = pct) 
# 
# Mat.Issue <- ag_dat |> dplyr::ungroup() |> dplyr::select(-period) |> as.matrix()





