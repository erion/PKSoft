CrCl_Alt <- function(peso,PCI,genero,idade,Cr,ASC,altura, PA){
  fator <- 0.85
  
  if(genero=="0"){
    fator <- 1
  }
  
  if(peso<PCI){
    PCI <- peso
  } else if(peso/PCI >= 1.3){
    PCI <- PA
  } else {
    PCI <- peso
  }
  
  crcl <- ((140 - idade) * (fator) * (PCI))/(72*Cr)
  return(crcl)
}