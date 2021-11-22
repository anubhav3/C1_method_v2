mcmc_burn_thin <- function(raw_result){
  
  t_length <- dim(raw_result$post_dists)[1]
  nlag <- lag_mcmc(raw_result$post_dists[(t_length/2+1):t_length,])
  
  n_take <- as.integer((t_length/2)/nlag)
  ind <- t_length/2 + nlag*(0:n_take)
  
  result <- list(acc_ss = raw_result$acc_ss[ind], dist = raw_result$dist[ind],
                 post_dists = raw_result$post_dists[ind,], total_sim = raw_result$total_sim)
  
  return(result)
}