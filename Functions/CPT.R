CPT <- function(k0,tinf,t,vdb,Cl){
  CPt <- 0
  if(t<=tinf){
    CPt <- (k0/Cl)*(1-exp(-(Cl/vdb)*t))
  } else {
    CPt <- (k0/Cl)*(1-exp(-(Cl/vdb)*tinf))*exp(-(Cl/vdb)*(t-tinf))
  }
  return(CPt)
}