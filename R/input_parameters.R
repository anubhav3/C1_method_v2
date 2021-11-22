## Other parameters
input_parameters <- function(){
  return(data.frame(tol = 0.9,
                                 N = 2500,
                                 n_cores = 1))
}



epsilon_t <- function(){
  final_tol <- input_parameters()$tol
  return(seq(0.95, final_tol, length = 5))
}
 
