PCI <- function(idade,altura,genero){
  peso_ideal <- 0
	if(idade > 18){
		delta <- (altura - 152.4)
		taxa_adicao <- (delta / 2.54)
		if(genero == 0){
			peso_ideal <- 50 + (2.3 * taxa_adicao)
		} else if (genero == 1){
			peso_ideal <- 45.5 + (2.3 * taxa_adicao)
		}
	} else if(idade <= 18){
		if(altura < 152.4){
			peso_ideal <- 2.396 * (exp(1)^(0.01863*altura))
		} else if (altura >= 152.4){
			if(genero == 0){
				peso_ideal <- 39 + ((2.27*(altura-152.4))/2.54)
			} else if (genero == 1){
				peso_ideal <- 42.2 + ((2.27*(altura-152.4))/2.54)
			}
		}
	}
	return(peso_ideal)
}