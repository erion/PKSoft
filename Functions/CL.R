CL <- function(ClCr,PCT,Cr,idade_meses,idade_gestacional,idade){
  cl <- 0
    if(idade_meses > 6){
       cl <- ((1.08 * ClCr) * 0.06) #f?rmula do Rafael
    } else {
      if(Cr <= 0.7){
        PNA <- 1
      } else {
        PNA <- 0
      }
      if(idade_gestacional <= 28){
        GA <- 0
      } else{
        GA <- 1
      }
      cl <- 0.006 + PCT * (0.028/Cr + 0.046355 * idade * PNA + 0.0123 * GA)
    }
  return(cl)
}
