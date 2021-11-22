## This function takes the model parameters and 
## body masses, and returns the
## vector of energies E
## matrix of handling times H
## and matrix of encounter rates L
Ratio.allometric.EHL <- function(M,
                                 e,
                                 r.a, r.b,
                                 a, ai, aj,
                                 n, ni=-3/4){
  
  ## The handling time function
  get.h <- function(Mi, Mj, r.a, r.b)
    ifelse((r.b-Mi/Mj)>0,
           r.a/(r.b-Mi/Mj),
           Inf)
  
  
  ## in matrix H resources are rows and consumers are columns
  if(!r.b==0)
    H <- outer(M, M, get.h, r.a, r.b)
  if(r.b==0)
    H = matrix(r.a, length(M),length(M))
  
  
  ## ENCOUNTER RATES: consumer - resource mass specific encounter rates
  get.a <- function(Mi, Mj,
                    a, ai, aj)
    a * Mi^ai * Mj^aj
  A <- outer(M, M, get.a,
             a=a, ai=ai, aj=aj)
  L <- A* n*M^ni
  
  ## Check if body sizes are sorted, as they need to be
  if(sum(order(M)==1:length(M))!=length(M))
    stop("Body sizes not sorted")
  
  ## energy values
  E <- e*M
  
  ## object to return
  list(E=E, H=H, L=L)
  
}