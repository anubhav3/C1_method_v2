lag_mcmc <- function(par){
  
  flag = 0
  lag_ind <- 500
  while(flag != 1){
    cor_res_acf <- acf(par, lag.max = lag_ind)$acf
    mat <- cor_res_acf <= 0.05
    cor_a <- mat[,1,1]
    cor_ai <- mat[,2,2]
    cor_aj <- mat[,3,3]
    cor_r.b <- mat[,4,4]
    
    if(min(sum(cor_a), sum(cor_ai), sum(cor_aj), sum(cor_r.b)) > 0){
      flag = 1
      ind_cor_a <- which.max(cor_a)
      ind_cor_ai <- which.max(cor_ai)
      ind_cor_aj <- which.max(cor_aj)
      ind_cor_r.b <- which.max(cor_r.b)
      
      lagg <- max(ind_cor_a, ind_cor_ai, ind_cor_aj, ind_cor_r.b)
    }
    else{
      lag_ind <- lag_ind + 100
    }
  }
  
  return(lagg)
  
}