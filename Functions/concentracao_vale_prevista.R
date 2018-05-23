concentracao_vale_prevista <- function(k0,duracao_intervalo,alfa,beta,intervalo_utilizado,vc,duracao_infusao){
  k21 <- 0.46
  (((k0/duracao_intervalo)*(k21-alfa)*(1-exp(alfa*duracao_intervalo))*(exp(-alfa*intervalo_utilizado)))/((vc*alfa)*(alfa-beta)*(1-exp(-alfa*intervalo_utilizado))))+(((k0/duracao_infusao)*(beta-k21)*(1-exp(beta*duracao_intervalo))*exp(-beta*intervalo_utilizado))/(vc*beta*(alfa-beta)*(1-exp(-beta*intervalo_utilizado))))
}

