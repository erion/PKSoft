CrCl <- function(peso,PCI,genero,idade,Cr,ASC,altura, PA){
	if(idade > 18){
		if(peso < PCI){
			PCI <- peso
		} else if(peso/PCI >= 1.3) {
		  PCI <- PA
		}

		if(genero == 0){
			crcl <- (PCI*(140 - idade))/(72*Cr)
		} else if(genero == 1){
			crcl <- (PCI*0.85*(140-idade))/(72*Cr)
		}
	} else if(idade <= 18){
		crcl <- 0.48 * (altura/Cr) * ASC
	}
return(crcl)
}
