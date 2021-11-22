#prints info about no. of simulations and acceptance in an output file
 
display_info_rej <- function(prop_acc_sim, time, dirnam, ncores){
  
  time_elapsed = round((as.numeric(time$toc)-as.numeric(time$tic))/60,3)
    
  pdf(paste(c(dirnam,'/',"display_info_rejection.pdf"), collapse = ''),width=5,height=5)
  a1 <- paste(c("Proportion of accepted simulations = ", prop_acc_sim), collapse = '')
  a2 <- paste(c("Time elapsed = ", time_elapsed, " min"), collapse = '')
  a3 <- paste(c("No. of cores = ", ncores), collapse = '')
  
  plot(NA, xlim=c(0,20), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,3,a1, pos=4)
  text(1,2,a2, pos=4)
  text(1,1,a3, pos=4)
  points(rep(1,3),1:3, pch=15)
  dev.off()
}


display_info_mcmc <- function(prop_acc_sim = prop_acc_sim, time = time, dirnam = dirnam){
  
  time_elapsed = round((as.numeric(time$toc)-as.numeric(time$tic))/60,3)
  
  pdf(paste(c(dirnam,'/',"display_info.pdf"), collapse = ''),width=5,height=5)
  a1 <- paste(c("Number of jumps = ", prop_acc_sim), collapse = '')
  a2 <- paste(c("Time elapsed = ", time_elapsed, " min"), collapse = '')
  
  plot(NA, xlim=c(0,20), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,2,a1, pos=4)
  text(1,1,a2, pos=4)
  points(rep(1,2),1:2, pch=15)
  dev.off()
}


display_info_smc <- function(time = time, dirnam = dirnam){
  
  time_elapsed = round((as.numeric(time$toc)-as.numeric(time$tic))/60,3)
  
  pdf(paste(c(dirnam,'/',"display_info.pdf"), collapse = ''),width=5,height=5)
  a1 <- paste(c("Time elapsed = ", time_elapsed, " min"), collapse = '')
  
  plot(NA, xlim=c(0,20), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,1,a1, pos=4)
  points(rep(1,1),1:1, pch=15)
  dev.off()
}
display_info_prop <- function(time = time, dirnam = dirnam){
  
  time_elapsed = round((as.numeric(time$toc)-as.numeric(time$tic))/60,3)
  
  pdf(paste(c(dirnam,'/',"display_info_prop.pdf"), collapse = ''),width=5,height=5)
  a1 <- paste(c("Time (computing prop.) = ", time_elapsed, " min"), collapse = '')
  
  plot(NA, xlim=c(0,20), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,2,a1, pos=4)
  points(rep(1,1),2, pch=15)
  dev.off()
}
