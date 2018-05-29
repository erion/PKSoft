intervalo_calculado <- function(concentracao_vale_desejada,Cl){
  concentracao_vale_desejada <- as.numeric(concentracao_vale_desejada)
  ic <- 6 * (72 / ((concentracao_vale_desejada * Cl) + 1.91))
  return(ic)
}
