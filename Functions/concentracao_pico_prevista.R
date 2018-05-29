concentracao_pico_prevista <- function(k0,duracao_infusao,alfa,beta,vc,intervalo_utilizado){
  k0 <- as.numeric(k0)
  duracao_infusao <- as.numeric(duracao_infusao)
  alfa <- as.numeric(alfa)
  beta <- as.numeric(beta)
  vc <- as.numeric(vc)
  intervalo_utilizado <- as.numeric(intervalo_utilizado)
  k21 <- 0.46
  (((k0/duracao_infusao)*(k21-alfa)*(1-exp(alfa*duracao_infusao))*(exp(-alfa*duracao_infusao)))/  ((vc*alfa)*(alfa-beta)*(1-exp(-alfa*intervalo_utilizado))))+(((k0/duracao_infusao)*(beta-k21)*(1-exp(beta*duracao_infusao))*exp(-beta*duracao_infusao))/(vc*beta*(alfa-beta)*(1-exp(-beta*intervalo_utilizado))))
}
