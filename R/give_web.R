give_web <- function(par, other_par){
  
  best.EHL <- Ratio.allometric.EHL(M=other_par$M,
                                    e=other_par$e,
                                    a=10^par$a, ai=par$ai, aj=par$aj,
                                    n=other_par$n, ni=other_par$ni,
                                    r.a=other_par$r.a, r.b=10^par$r.b)
  best.web <- Get.web(best.EHL)
  
  return(best.web)
}

