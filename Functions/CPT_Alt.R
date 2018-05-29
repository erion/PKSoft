CPT_Alt <- function(k0,a,tinf,t,b,Vc,vdb,dose){
  k21 <- 0.46

  S <- 1
  if(t < tinf){
    CPt <-
      (
        (
          (dose/tinf)
          *
            (k21-a)
          *
            (1-exp(a*t))
          *
            (exp(-a*t))
        )
        /
          (
            (Vc*a)
            *
              (a-b)
          )
      )
    +
      (
        (
          (dose/tinf)
          *
            (b-k21)
          *
            (1-exp(b*t))
          *
            (exp(-b*t))
        )
        /
          (Vc*b*(a-b))
      )
  } else {
    CPt <- (((dose/tinf)*(k21-a)*(1-exp(a*tinf))*(exp(-a*t)))/((Vc*a)*(a-b))) + (((dose/tinf)*(b-k21)*(1-exp(b*tinf))*(exp(-b*t)))/(Vc*b*(a-b)))
  }
  return(CPt)
}