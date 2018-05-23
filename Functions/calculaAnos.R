calculaAnos <- function(date){
  nascimento <- as.POSIXlt(date)
  today <- as.POSIXlt(Sys.Date())
  idade_anos <- round(as.numeric(Sys.Date() - as.Date(date)) / 365)
  return(idade_anos)
}
