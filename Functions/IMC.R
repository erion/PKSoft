IMC <- function(PCT,altura){
	altura <- altura/100
	imc <- PCT/(altura^2)
	return(imc)
}