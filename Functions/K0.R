K0 <- function(concentracao_vale_desejada,alfa,beta,vc,duracao_infusao,intervalo_utilizado){
  K21 <- 0.46
  k0 <- concentracao_vale_desejada /((((( K21- alfa)*(1-exp(alfa* duracao_infusao))*exp(-alfa*intervalo_utilizado))/((vc*alfa*(alfa- beta)*(1-exp(-alfa* intervalo_utilizado))))))+((((beta- K21)*((1-exp(beta* duracao_infusao))*exp(-beta* intervalo_utilizado))/(vc*beta*(alfa-beta)*(1-exp(-beta* intervalo_utilizado)))))))
}
