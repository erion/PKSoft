PD <- function(PCI,peso){
	if(peso<=PCI){
		peso_dosagem <- PCI
	} else if(peso>PCI){
		peso_dosagem <- PCI + (0.4*(peso-PCI))
	}
return(peso_dosagem)
}