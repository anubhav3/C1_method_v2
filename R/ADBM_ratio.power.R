## the function that takes the parameters to optimise in opt and
## returns the web
ratio.power <- function(opt, x){

  a = as.numeric(opt["a"])
  ai = as.numeric(opt["ai"])
  aj = as.numeric(opt["aj"])
  r.b = as.numeric(opt["r.b"])
  
  e=x[["e"]]
  n=x[["n"]]
  ni=x[["ni"]]
  r.a=x[["r.a"]]
  M=x[["M"]]
  S=x[["S"]]
  real.web <- x[["real.web"]]
  
  EHL <- Ratio.allometric.EHL(M=M,
                              e=e,
                              a=a, ai=ai, aj=aj,
                              n=n, ni=ni,
                              r.a=r.a, r.b=r.b)
  
  web <- Get.web(EHL)
  return(web)
}

ratio.power_exp <- function(opt, x){
  
  a = 10^as.numeric(opt["a"])
  ai = as.numeric(opt["ai"])
  aj = as.numeric(opt["aj"])
  r.b = 10^as.numeric(opt["r.b"])
  
  e=x[["e"]]
  n=x[["n"]]
  ni=x[["ni"]]
  r.a=x[["r.a"]]
  M=x[["M"]]
  S=x[["S"]]
  real.web <- x[["real.web"]]
  
  EHL <- Ratio.allometric.EHL(M=M,
                              e=e,
                              a=a, ai=ai, aj=aj,
                              n=n, ni=ni,
                              r.a=r.a, r.b=r.b)
  
  web <- Get.web(EHL)
  return(web)
}