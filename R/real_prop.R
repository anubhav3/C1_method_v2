real_prop <- function(all.web.info){
  
  title <- all.web.info$web.name
  community <- mat.to.comm(all.web.info$predation.matrix, title)
  connectance <- sum(all.web.info$predation.matrix)/(dim(all.web.info$predation.matrix)[1]^2)
  prop_basal <- FractionBasalNodes(community)
  prop_inter <- FractionIntermediateNodes(community)
  prop_top <- FractionTopLevelNodes(community)
  without_basal <- RemoveNodes(community, BasalNodes(community))
  prop_herb <- length(which(IsBasalNode(without_basal)))/dim(community$nodes)[1]
  mean_trop_lvl <- -999
  # mtl <- try(mean(ShortWeightedTrophicLevel(community)))
  # mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else -999
  max_trop_lvl <- -999
  # maxtl <- try(max(LongestTrophicLevel(community)))
  # max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else -999
  mean_omn <- FractionOmnivorous(community)
  clus_coeff <- ClustF(all.web.info$predation.matrix, type = "directed")$GlobaltotalCC
  # clus_temp <- try(ClustF(all.web.info$predation.matrix, type = "directed"))
  # clus_coeff <- if(is.numeric(clus_temp) == TRUE) clus_temp$GlobaltotalCC else -999
  sd_gen <- sd(InDegree(community))
  sd_vulner <- sd(OutDegree(community))
  
  community_without_diag <- TrophicSimilarity(community)
  diag(community_without_diag) <- -999
  diet_sim <- max(community_without_diag)
  
  mean_path_lt <- mean(ShortestPaths(community))
  
  real_prop_f <- data.frame(connectance=connectance,prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                            prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                            mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                            sd_vulner = sd_vulner, diet_sim = diet_sim, mean_path_lt = mean_path_lt)
  
  return(real_prop_f)
}