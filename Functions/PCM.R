PCM <- function(genero,PCT,IMC){
	if(genero == 0){
		pcm <- (9270*PCT)/(6680+(216*IMC))
	} else if(genero ==1) {
		pcm <- (9270*PCT)/(8780+(244*IMC)) 
	}
return(pcm)
}