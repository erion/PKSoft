CPT <- function(k0,tinf,t,vdb,Cl){
  k0 <- as.numeric(k0)
  tinf <- as.numeric(tinf)
  t <- as.numeric(t)
  vdb <- as.numeric(vdb)
  Cl <- as.numeric(Cl)
  CPt <- 0
  if(t<=tinf){
    CPt <- (k0/Cl)*(1-exp(-(Cl/vdb)*t))
  } else {
    CPt <- (k0/Cl)*(1-exp(-(Cl/vdb)*tinf))*exp(-(Cl/vdb)*(t-tinf))
  }
  return(CPt)
}