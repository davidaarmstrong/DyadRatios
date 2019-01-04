iscorr <-
function(issue,mood) { #compute issue-scale correlations
  Nv<- length(issue[1,])
  Np<- length(issue[,1])
  Rvector<- numeric(Nv)
  for (v in 1:Nv) {
    N<- Np - sum(is.na(issue[,v]))
    if (N > 1) Rvector[v]<- cor(issue[,v],mood[3,],use="complete.obs",method="pearson")
    }
  return(Rvector)
  }
