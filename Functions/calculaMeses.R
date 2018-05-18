calculaMeses <- function(date){
  nascimento <- as.POSIXlt(date)
  today <- as.POSIXlt(Sys.Date())
  idade_meses <- round(as.numeric(Sys.Date() - as.Date(date)) / 12) 
  return(idade_meses)
}
