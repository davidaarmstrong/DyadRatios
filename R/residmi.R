residmi <-
function(issue,v,mood) { #function regresses issue(v) on mood and then residualizes it
o<- lm(issue[,v] ~ mood[3,]) #regress issue on mood to get a,b
issue[,v]<- 100 + issue[,v] - (o$coef[1]+o$coef[2]*mood[3,]) #100 + Y - (a+bx)
return(issue[,v])
}
