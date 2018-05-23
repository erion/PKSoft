VDB <- function(idade_meses,PD,cl,vc,peso,crcl,idade){
  vdb <- 0
  k21 <- 0.46
  if(idade_meses > 6){
    if(idade >= 18 && crcl < 10){
      vdb <- 1.0 * PD
    } else {
      vdb <- 0.7 * PD
    }
  } else {
    k10 = cl/vc
    clq = 0.0334 * peso
    k12 = clq/vc
    a0 = k10 * k21
    a1 = -(k10+k12+k21)
    b = (-a1-sqrt(a1^2 - (4 * a0)))/2
    vdb = cl/b
  }
  return(vdb)
}
