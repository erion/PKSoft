sobreposicao <- function(curva,intervalo_utilizado, qtd){
  qtd_doses <- qtd
  inicio_dose <- 0
  ultimate_vetor <- integer(1200)
  
  for(i in 1:1200){
    ultimate_vetor[i] <- as.numeric(curva[i])
  }
   
  for(i in 1:qtd_doses){
    inicio_dose <- inicio_dose + intervalo_utilizado * 10
    for(j in inicio_dose:1200){
      ultimate_vetor[j] <- ultimate_vetor[j] + curva[j - inicio_dose + 1]
    }
  }
  return(ultimate_vetor)
}