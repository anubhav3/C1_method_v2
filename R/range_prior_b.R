# The range of the prior distribution is dependent on the body sizes

range_prior_b <- function(M){
  
  # foodweb_data <- load(paste("data/", web.to.analyse, ".web.Rdata", sep=""))
  # all.web.info <- get(foodweb_data)
  # 
  # M <- all.web.info$species.sizes
  M_ratio <- outer(M, M, FUN = "/")
  M_ratio <- as.vector(M_ratio)
  
  min_M_ratio <- min(M_ratio)
  max_M_ratio <- max(M_ratio)
  
  return(round(log10(max_M_ratio)))
}