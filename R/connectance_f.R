connectance_f <- function(ss_sim = ss_sim){
  S <- dim(ss_sim)[1]
  connectance_f_r <- sum(ss_sim)/(S^2)
  
  return(connectance_f_r)
}