#Private function, used by class.sum()
kappa=function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}
