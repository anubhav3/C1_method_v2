mcmc_chk_conv <- function(input){
  
  t_length <- dim(input[[1]]$post_dists)[1]
    
  l1 <- input[[1]]$post_dists[(t_length/2+1):t_length,]
  l2 <- input[[2]]$post_dists[(t_length/2+1):t_length,]
  l3 <- input[[3]]$post_dists[(t_length/2+1):t_length,]
  
  
  l1 <- as.mcmc(l1)
  l2 <- as.mcmc(l2)
  l3 <- as.mcmc(l3)
  
  l_mcmc <- mcmc.list(l1,l2,l3)
  result <- gelman.diag(l_mcmc)
  
  return(result)
}