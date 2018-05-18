PA <- function(peso,PCI,PCM){
	if(peso <= PCI){
		pa <- peso
	} else if(peso>PCI){
		pa <- PCM
	}
return(pa)
}