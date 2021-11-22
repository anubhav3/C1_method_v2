#Computes proportion of basal species in a food web with using the predation matrix (where, column represents consumer and row represents resources)

prop_basal_inter_top_herb <- function(pred_mat){
  nspecies <- dim(pred_mat)[1]
  var_colsum <- colSums(pred_mat)
  var_rowsum <- rowSums(pred_mat)
  
  zero_colsum <- which(var_colsum == 0)
  zero_rowsum <- which(var_rowsum == 0)
  
  singleton_species <- intersect(zero_colsum, zero_rowsum) #node which has no links including itself
  basal_species <- setdiff(zero_colsum, zero_rowsum)
  top_species <- setdiff(zero_rowsum, zero_colsum)
  
  without_basal_species <- pred_mat[-zero_colsum, -zero_colsum]
  nspecies_2 <- dim(without_basal_species)[1]
  
  var_colsum_2 <- colSums(without_basal_species)
  var_rowsum_2 <- rowSums(without_basal_species)
  zero_colsum_2 <- which(var_colsum_2 == 0)
  zero_rowsum_2 <- which(var_rowsum_2 == 0)
  herb_species <- setdiff(zero_colsum_2, zero_rowsum_2)
  
  n_basal <- length(basal_species)
  n_top <- length(top_species)
  n_inter <- nspecies - n_basal - n_top
  n_singleton <- length(singleton_species)
  n_herbivores <- length(herb_species)
  
  prop_basal <- n_basal/nspecies
  prop_top <- n_top/nspecies
  prop_inter <- n_inter/nspecies
  prop_herbivores <- n_herbivores/nspecies
  
  return(list(prop_basal = prop_basal, prop_inter = prop_inter, 
              prop_top = prop_top, n_singleton = n_singleton,
              prop_herb = prop_herbivores))
}