dist_ACC <- function(ss_sim, ss_real){
  TP <- sum(ss_sim==ss_real & ss_real==1)
  TN <- sum(ss_sim==ss_real & ss_real==0)
  P <- sum(ss_real==1)
  N <- sum(ss_real==0)
  
  acc <- (TP+TN)/(P+N)
  dist_ACC_r <- 1-acc
  
  return(dist_ACC_r)
  
}