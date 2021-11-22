dist_TSS <- function(ss_sim, ss_real){
  a <- sum(ss_sim==1 & ss_real==1)
  b <- sum(ss_sim==1 & ss_real==0)
  c <- sum(ss_sim==0 & ss_real==1)
  d <- sum(ss_sim==0 & ss_real==0)
  
  TSS_func <- (a*d-b*c)/((a+c)*(b+d))
  dist_TSS_r <- 1-TSS_func
  if(is.nan(dist_TSS_r)==TRUE){dist_TSS_r <- 1000}
  
  return(dist_TSS_r)
}
