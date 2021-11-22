Get.web <- function(EHL, energy.intake=F){

  S <- length(EHL[[1]])
  
  web <- matrix(0, S, S)
  overall.energy <- numeric(S)
  per.species.energy <- matrix(0, S, S)
  
  ## in matrix P, columns are cosumers and contain profit of that consumer
  ## feeding on each prey (row)
  P <- EHL[[1]]/EHL[[2]]
  
  ## split code depending on whether encounter rates are predator specific or not
  if(is.matrix(EHL[[3]])){
    for(j in 1:S){
      
      p <- P[,j]
      
      if(sum(p>0)==1)
        web[which(p>0),j] <- 1
      
      if(sum(p>0)>1){
        
        ## ordering of p required
        
        order.by.p <- order(p, decreasing=T)
        p <- p[order.by.p]
        Lj <- EHL[[3]][,j][order.by.p]
        hj <- EHL[[2]][,j][order.by.p]
        Ej <- EHL[[1]][order.by.p]
        
        cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum( Lj * hj))
        
        dj <- max(which(cumulative.profit==max(cumulative.profit)))
        
        web[,j] <- c(rep(1, dj), rep(0, S-dj))[order(order.by.p)]
        
        overall.energy[j] <- cumulative.profit[dj]
        
        energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
        all.energies <- c(energies, rep(0, S-length(energies)))
        
        per.species.energy[,j] <- all.energies[order(order.by.p)]
        
      }
    }
  }
  
  ## This if encounter rates are not predator specific
  if(is.vector(EHL[[3]])){
    for(j in 1:S){
      
      if(sum(p>0)==1)
        web[which(p>0),j] <- 1
      
      if(sum(p>0)>1){
        
        ## ordering of p required
        p <- P[,j]
        order.by.p <- order(p, decreasing=T)
        p <- p[order.by.p]
        Lj <- EHL[[3]][order.by.p]
        hj <- EHL[[2]][,j][order.by.p]
        Ej <- EHL[[1]][order.by.p]
        
        cumulative.profit <- cumsum(Ej * Lj) / (1 + cumsum(Lj * hj))
        
        dj <- max(which(cumulative.profit==max(cumulative.profit)))
        web[,j] <- c(rep(1, dj), rep(0, S-dj))[order(order.by.p)]
        
        overall.energy[j] <- cumulative.profit[dj]    
        
        energies <- c(Ej * Lj)[1:sum(web[,j])] / (1 + cumsum( Lj * hj)[sum(web[,j])]) 
        all.energies <- c(energies, rep(0, S-length(energies)))
        
        per.species.energy[,j] <- all.energies[order(order.by.p)]
        
      }
    }
  }
  
  if(energy.intake)
    result <- list(web=web, overall.flux=overall.energy, per.species.flux=per.species.energy)
  else
    result <- web
  result
  
}