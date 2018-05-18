VC <- function(peso,idade_meses,PD,idade,crcl){
  PCT <- peso
  vc <- 0
  if(idade_meses <= 6){
    vss <- 0.793 * PCT + 0.01
    vc <- 0.666 * vss
  } else {
    if(idade >= 18 && crcl < 10){
      vc <- 0.45* PD
    } else{
      vc <- 0.21 * PD
#      vc <- 0.17 * PD      F?rmula do Rafael
    }
  }
  return(vc)
}
