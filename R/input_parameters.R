## Other parameters
input_parameters <- function(){
  return(data.frame(tol = 2,
                                 N = 1e6,
                                 n_cores = 5))
}



epsilon_t <- function(){
  final_tol <- input_parameters()$tol
  return(seq(0.95, final_tol, length = 5))
}
 
