PCM <- function(genero,PCT,IMC){
	if(genero == 'M'){
		pcm <- (9270*PCT)/(6680+(216*IMC))
	} else if(genero == 'F') {
		pcm <- (9270*PCT)/(8780+(244*IMC))
	}
return(pcm)
}