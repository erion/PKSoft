intervalo_calculado <- function(concentracao_vale_desejada,Cl){
  ic <- 6 * (72 / ((concentracao_vale_desejada * Cl) + 1.91))
  return(ic)
}
