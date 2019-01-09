extract <-
function(varname,date,index,ncases=NULL,unit="A",mult=1,begindt=NA,enddt=NA,npass=1,smoothing=TRUE,endmonth=12, plot=FALSE) {
  formula<-match.call(extract)
  csign <- NULL
  nrecords<- length(varname)
  if (is.null(ncases)) ncases<- rep(0,nrecords)
  moddate<- date #create temporary date vector, leaving original unmodified
  if ((unit=="A" || unit=="O") && endmonth<12) {
    for (i in 1:nrecords) { #first loop through raw data file
      month<- findmonth(moddate[i])
      year<- findyear(moddate[i])
      if (month>endmonth) moddate[i]<- ISOdate(year+1,1,1) #modified date become 1/1 of next year
    } #end loop through data
  } # end if

  if (is.na(begindt)) minper<-findmonth(min(moddate)) else minper<-findmonth(begindt)
  if (is.na(begindt)) miny<-findyear(min(moddate)) else miny<-findyear(begindt)
  if (is.na(begindt)) minday<-findday(min(moddate)) else minday<-findday(begindt)
  if (is.na(enddt)) maxper<-findmonth(max(moddate)) else maxper<-findmonth(enddt)
  if (is.na(enddt)) maxy<-findyear(max(moddate)) else maxy<-findyear(enddt)
  if (is.na(enddt)) maxday<-findday(max(moddate)) else maxday<-findday(enddt)
  if (unit=="Q") {
    minper<- as.integer((minper-1)/3)+1
    maxper<- as.integer((maxper-1)/3)+1
  }
  mindate<- ISOdate(miny,minper,minday,0,0,0,tz="GMT")
  maxdate<- ISOdate(maxy, maxper, maxday,0,0,0,tz="GMT") #86400=24*60*60

  #SETCONS:
  latent<- numeric(1)
  aggratio<- 0
  fb<- 1 #initialize
  auto<- "start"  #meaningless value
  alpha<- 1
  alpha1<- 1
  pass<- 1
  holdtola<- 0.001
  tola<- holdtola
  iter<- 0
  lastconv<- 99999
  wtmean<- 0 #for it=1
  wtstd<- 1
  fract<- 1

  if (unit=="A") {
    nperiods<- maxy-miny+1
    aggratio<- 1
    months<- 12
  }
  if (unit=="O") {
    years<- mult
    months<- years*12
    aggratio<- 2
    odd<- (maxy-miny+1) %% mult  #mod
    nperiods=as.integer((maxy-miny)/mult) + odd
  }
  if (unit=="M") {
    fract<- 100
    nperiods<- (maxy-miny)*12
    nperiods<- nperiods-12 + (12-minper+1) + maxper
    aggratio<- 1/12
    months<- 1
  }
  if (unit=="Q") {
    aggratio<- 1/4
    months<- 3
    nperiods<- as.integer((maxy-miny)/aggratio)
    nperiods<- nperiods-4 + (4-minper+1) + maxper
    fract<- 10
  }
  if (unit=="D") {
    months=1
    nperiods<- (as.integer(maxdate)-as.integer(mindate))/86400 + 1 #86400=24*60*60
  }

  arinv<- 1/aggratio
  aggratio<- months/12
  nrecords<- length(index)

#HERE WE SET UP FUNDAMENTAL DIMENSIONS AND DECLARE VECTORS
  if (fb != 2)  mood<- array(dim=c(3,nperiods))
  vfac<- factor(varname) #make a factor vector
  vlev<- levels(vfac)    #find unique categories
  nvar<- length(vlev)    #how many are there?, includes unusable series
  valid<- numeric(nvar)
  csign<- numeric(nvar)
  assign("csign", numeric(nvar), envir=parent.frame())
  vl<- character(nvar)
  r<- numeric(nvar)
  oldr<- rep(1,nvar) # r=1 for all v initially

  issue<- array(dim=c(nperiods,nvar))
  count<- numeric(nperiods)
  vl<- numeric(nvar)
  period<- numeric(nperiods)
  converge<- 0
  evalue<- 0

  # create numeric variable period, eg, yyyy.0m 
  if (unit=="D") {
    period<-seq(1:nperiods) 
    } else {
  if (months >= 12) {
    for (l in 1:nperiods) { 
      p <- (l - 1) * aggratio
      period[l] <- miny + p
      } #next l
   } else {
     y <- 0
     i <- 0
     my <- miny
     if (minper == 1)  my <- my - 1
     for (l in 1:nperiods) { 
       i<- 1 + ((l-1) %% arinv)
       mq <- minper + i - 1
       mq<- 1 + ((mq-1) %% arinv) 
       if (mq == 1)  y <- y + 1 #first month or quarter, increment year
       period[l] <- my + y + mq / fract
     } # end for
   } #end else
  } # end if


  agglist<- aggregate(varname,moddate,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) # call aggregate to produce issue matrix
  vl<- agglist$lab #extract two elements of the list from aggregate call
  issue<- agglist$iss
  rm(agglist) #don't need this anymore

  #NOW REDUCE ISSUE MATRIX TO ELIMINATE UNUSABLE SERIES (WN<2)
  ndrop<- 0
  nissue<- numeric(nperiods)
  std<- numeric(nperiods)
  for (v in 1:nvar) {
    std[v]<- 0 #default
    nissue[v]<- sum(!is.na(issue[,v])) #criterion is 2 cases for npass=1 or 3 for npass=2
    if (nissue[v]>npass) std[v]<- sqrt(var(issue[,v],na.rm=TRUE)) #this is just a test for variance >0
    if (std[v]<.001) {  #case dropped if std uncomputable (NA) or actually zero (constant)
      ndrop<- ndrop+1
      print(paste("Series",vl[v],"discarded.  After aggregation cases =",nissue[v]))
      }
    }
  nvarold<- nvar
  nvar<- nvar-ndrop
  pointer<- 1
  found<- FALSE
  
  for (v in 1:nvar) { #now reduced nvar
    while (found==FALSE && pointer<=nvarold) { #find first valid column and push down
      if (std[pointer]>.001) { #good case, transfer
         issue[,v]<- issue[,pointer]
         vl[v]<- vl[pointer]
         pointer<- pointer+1
         found<- TRUE
      } else {
        pointer<- pointer+1 #bad case, increment pointer
      } #end if
    } #end while
    found<- FALSE
  } #for
  length(vl)<- nvar #reduce  
  length(issue)<- nperiods*nvar  #chop off unused columns
  attr(issue,"dim")<- c(nperiods,nvar)
  N<- numeric(nvar)

#export<<-list(nperiods,nvar,issue)


  for (pass in 1:npass) { #newpass: RESTART FOR SECOND DIMENSION CASE
    if (pass == 2) { #reset iteration control parameters
      iter <- 0
      tola = holdtola
      lastconv <- 99999
      converge<- lastconv
      conv<- converge
    } else {
      av<- numeric(nvar)
      std<- numeric(nvar)
#      ngood<- 0
      for (v in 1:nvar) { #compute av and std by issue nvar now reduced to good cases
        wn<- as.integer(nperiods-sum(is.na(issue[,v])))
        av[v] <- mean(issue[,v],na.rm=TRUE)
        std[v]<- sqrt(var(issue[,v],na.rm=TRUE) * ((wn - 1)/wn)) #convert to population standard deviation
        issue[,v]<- 100 + 10 * (issue[,v] - av[v])/std[v]  #standardize
#        ngood<- ngood+1
      }#end for
    }
    #READY FOR ESTIMATION, SET UP AND PRINT OPTIONS INFO     
    out<- as.character(10) #initial length only
    out[1]<- print(paste("Estimation report:"))
    if (pass == 1) {
      if (months >= 12) {
        out[2]<- print(paste("Period:", miny, " to", maxy,"     ", nperiods, " time points"))
      } else {
        out[2]<- print(paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points"))
      }
      out[3]<- print(paste("Number of series: ", nvar+ndrop))
      out[4]<- print(paste("Number of usable series: ", nvar))
      out[5]<- print(paste("Exponential smoothing: ",smoothing))
    }
    out[6]<- print(paste("Iteration history: Dimension ",pass))
    print(" ")
    out[7]<- print("Iter Convergence Criterion Reliability Alphaf Alphab")
    outcount<- 7
    
    for (p in 1:nperiods) {
      count[p]<- sum(!is.na(issue[p,]))
    }
    valid<- rep(1,times=nvar)
    csign<- rep(1,times=nvar)
    assign("csign", rep(1,times=nvar), envir=parent.frame())
    auto <- "y"              #iterative estimation on by default
    quit <- 0                #false implies go ahead and estimate

    while (iter == 0 || converge > tola) {   #MASTER CONTROL LOOP WHICH ITERATES UNTIL SOLUTION REACHED

      for (fb in 1:2) { #    MASTER fb LOOP       fb=1 is forward, 2 backward 
        dominate.out<- dominate(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha, csign)  #master estimation routine
        alpha1<- dominate.out$alpha1
        alpha<- dominate.out$alpha
        mood[fb,]<- dominate.out$latent
        } #next fb

    fb <- 3 #average mood from here on
    for (p in 1:nperiods) { #    AVERAGE
      mood[fb, p] <- (mood[1, p] + mood[2, p]) / 2
    } #next p
    moodmean<-mean(mood[3,])
    sdmood<-sd(mood[3,])
    for (p in 1:nperiods) {  #PLACEMENT OF THIS LOOP MAY NOT BE RIGHT
      mood[fb,p] <- ((mood[fb,p] - moodmean) * wtstd / sdmood) + wtmean
    } #end for

    if(plot){
      #plot commands
      t<- seq(1:nperiods) #time counter used for plot below
      lo<- 50 #force scale of iterative plot to large range
      hi<- 150
      if (min(mood[3,]) < lo) lo=min(mood[3,]) #whichever larger, use
      if (max(mood[3,]) > hi) hi=max(mood[3,])
      dummy<- rep(lo,nperiods) #dummy is fake variable used to set plot y axis to 50,150
      dummy[nperiods]<- hi
      if (iter==0) {
        plot(t,dummy,type="l",lty=0,xlab="Time Period",ylab="Estimate by iteration",main="Estimated Latent Dimension") #create box, no visible lines
        } else {
        lines(t,mood[3,],col=iter)
      }  
    }
    iter <- iter + 1 
    if (auto == "y") r<- iscorr(issue,mood) else auto <- "y"   #recompute correlations

    wtmean<- 0
    wtstd<- 0
    vsum<- 0
    goodvar<- 0
    converge<- 0 #start off default
    evalue<- 0
    totalvar<- 0

    assign("csign", ifelse(is.na(sign(r)), csign, sign(r)), envir=parent.frame())
    for (v in 1:nvar) {
      wn<- nperiods-sum(is.na(issue[,v]))
      #if (!is.na(sign(r[v]))) csign[v]<<- sign(r[v])
      wn<- nperiods-sum(is.na(issue[,v]))
      if (wn>1) { #sum over variables actually used
        vratio <- wn / nperiods
        evalue <- evalue + vratio * r[v]^2
        totalvar <- totalvar + vratio
      } #end if
  
      #convergence tests
      if (wn > 3) {
       conv <- abs(r[v] - oldr[v])      #conv is convergence test for item=v
       conv <- conv * (wn / nperiods)                #weight criterion by number of available periods
       if (conv > converge)  converge <- conv        #converge is the global max of conv
      } #end if
      if (!is.na(r[v])) oldr[v] <- r[v]
      if (!is.na(r[v])) valid[v] <- r[v]^2
      if (!is.na(av[v])) wtmean <- wtmean + av[v] * valid[v]
      if (!is.na(std[v])) wtstd <- wtstd + std[v] * valid[v]
      if (!is.na(r[v])) vsum <- vsum + valid[v]
    } #end v loop

    if (vsum > 0)  wtmean <- wtmean / vsum
    if (vsum > 0)  wtstd <- wtstd / vsum
    if (pass == 1) {
      mean1 <- wtmean
      std1 <- wtstd
      e1=evalue
    } else {
      wtmean <- mean1
      wtstd <- std1 #*unexp
    } #end if
    fbcorr <- cor(mood[1,],mood[2,]) #fnfrontback 

    if (quit != 1) {
      outcount<- outcount+1
      cv<- format(round(converge,4),nsmall=4) 
      itfmt<-format(round(iter),justify="right",length=4)
      out[outcount]<- print(paste(itfmt,"       ",cv,"   ",round(tola,4),"    ",round(fbcorr,3),round(alpha1,4),round(alpha,4)))
      }
    if (converge > lastconv)  tola <- tola * 2
    lastconv <- converge

    auto = "y"  #skip corr on iter=1, set auto on

    if (iter >= 50) break #get out of while loop
    } #END MASTER WHILE ITERATION CONTROL LOOP

    if (auto == "y" && converge<tola) { #IF WE REACH THIS CODE WE HAVE A FINAL SOLUTION TO BE REPORTED
    if (pass == 1) out1<- out #hold output for 2 dimensional solution
    auto <- "Q"
    quit <- 1                #flag solution reached, last time through
    r<- iscorr(issue,mood)   #final iteration correlations
    if (pass == 1) r1<- r #hold correlations for 2 dimensional solution

    if (pass > 1) {
      unexp <- totalvar 
      totalvar <- unexp * totalvar
      evalue <- evalue * unexp
    } #    end if

    if (pass == 1) {
      expprop <- evalue / totalvar
      tot1 <- totalvar
    } else {
      erel <- evalue / totalvar          #% exp relative
      totalvar <- (1 - expprop) * tot1   #true var=original var discounted by %exp
      evalue <- erel * totalvar          #rescale to retain %exp relationship
      expprop <- evalue / tot1           #now reduce eral to expprop
    } #    end if

    for (v in 1:nvar) {
      N[v]<- sum(!is.na(issue[,v]))
      }
    var.out<- list(varname=vl,loadings=r,means=av,std.deviations=std)
  
    print(" ")  
    outcount<- outcount+1
    out[outcount]<- print(paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2)))  
    outcount<- outcount+1
    out[outcount]<- print(paste("  Percent Variance Explained: ",round(100 * expprop,2)))

    if (pass !=  2 && npass>1) {
      for (v in 1:nvar) { 
        valid[v] <- 0               #reset all, regmoodissue will set good=1
        if (csign[v] != 0)  issue[,v]<- residmi(issue,v,mood)   #regmoodissue()
       } #v loop
    }  # if
    #begin prn output routine # mood[fb,] is now our estimate,    WHAT ABOUT A SECOND DIMENSION
    latent<- mood[fb,] #vector holds values for output
    if (pass == 1) latent1<- latent #hold first dimension
    print(" ")
    out[outcount+1]<- print(paste("Final Weighted Average Metric:  Mean: ",round(wtmean,2)," St. Dev: ",round(wtstd,2)))
    #for Zelig output
    if (npass==1) {
      extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1)
    } else {
    for (i in 6:outcount) {
      out[i-5]=out[i]
    }
    length(out)<- outcount-5
    extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1,setup2=out,loadings2=r,latent2=latent)
    }
    } #end if auto="y" 
  } #end of for pass=1,2 loop 

  par(col=1) #reset on termination
  class(extract.out)<- "extract"
  return(extract.out)
  }
