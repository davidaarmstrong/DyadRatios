esmooth <-
function(mood, fb, alpha){ 
##########################################################################################
smooth<- function(alpha) { #for time series "series" and alpha "alpha[1]" compute sum of squared forecast error
ferror<- numeric(1)
T<- length(series)
xvect<- numeric(T)        
xvect[1] <-  series[1]
for (t in 2:T) { 
  xvect[t] <-  alpha[1] * series[t] + (1 - alpha[1]) * xvect[t - 1]
}
sumsq <-  0
for (t in 3:T) { 
  ferror <-  series[t] - xvect[t - 1]
  sumsq <-  sumsq + ferror ^ 2
} 
return(sumsq) #this is the value of the function for a particular parameter alpha[1]
} # END OF FUNCTION SMOOTH   
##########################################################################################

series<- mood[fb,] #create series to be smoothed
sm.out<- optim(c(.75),smooth,method="L-BFGS-B",lower=0.5,upper=1)  #call smoother
alpha<- sm.out$par                          #assign result to alpha
#NOW SMOOTH USING ALPHA
T<- length(series)
for (t in 2:T) { 
  mood[fb,t] <-  alpha * series[t] + (1 - alpha) * mood[fb,t - 1]
}
return(alpha)
}
