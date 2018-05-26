AUIC <- function(ASC,intervalo_calculado){
  CIM <- 2
  auic <- (ASC * 24) / (CIM * intervalo_calculado)
  return(auic)
}

