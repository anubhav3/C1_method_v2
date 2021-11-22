## Calculates True Skill Statistic
TSS_func <- function(ss_sim, ss_real){
  a <- sum(ss_sim==1 & ss_real==1)
  b <- sum(ss_sim==1 & ss_real==0)
  c <- sum(ss_sim==0 & ss_real==1)
  d <- sum(ss_sim==0 & ss_real==0)
  
  result <- (a*d-b*c)/((a+c)*(b+d))
  return(result)
}