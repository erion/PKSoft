CL_Alt <- function(ClCr,PCT,Cr,idade_meses,idade_gestacional,idade){
  cl <- (1.08 * ClCr) * 0.06
  return(cl)
}