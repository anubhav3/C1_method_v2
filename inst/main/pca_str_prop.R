# 21.11.2021
# I implement principal component analysis for the food web properties

fw_metadata <- readxl::read_excel("data/parameter_values.xlsx")
PCA_df <- data.frame(foodweb = character(), PCA1 = double(), PCA2 = double(), PCA3 = double(), conn_cor = double())
ind <- 1
for(fw_name in fw_metadata$foodweb){
  tol <- fw_metadata$dist_rej[ind]
  fname <- paste("results/rejection/", fw_name, "/rN=1000_tol=", tol, "_TSS_lower_a/", fw_name, "_prop.RDS", sep = "" )
  fw_prop_org <- readRDS(file = fname)$prop
  
  fw_prop <- data.frame(connectance = fw_prop_org$connectance, prop_basal = fw_prop_org$prop_basal,
                        prop_inter = fw_prop_org$prop_inter, prop_top = fw_prop_org$prop_top,
                        prop_herb = fw_prop_org$prop_herb,
                        clus_coeff = fw_prop_org$clus_coeff, sd_gen = fw_prop_org$sd_gen,
                        sd_vulner = fw_prop_org$sd_vulner, diet_sim = fw_prop_org$diet_sim,
                        nest_prop = fw_prop_org$nest_prop)
  
  pca <- prcomp(x = fw_prop, center = TRUE, scale. = TRUE)
  smry <- summary(pca)
  PCA <- as.numeric(smry$importance["Proportion of Variance",])
  PCA123 <- as.numeric(smry$importance["Cumulative Proportion",])[3]
  
  conn_cor <- cor(smry$x[,1], fw_prop$connectance, method = "pearson")
  PCA_df <- rbind(PCA_df,
                  data.frame(foodweb = fw_name, PCA1 = round(PCA[1],2), PCA2 = round(PCA[2],2), PCA3 = round(PCA[3],2), 
                             PCA123 = round(PCA123,2),
                             conn_cor = round(conn_cor,2)))
  
  ind <- ind + 1
}

dd_PCA <- PCA_df[c(1,2,3,4,6)]
dd_PCA <- rbind(dd_PCA, data.frame(foodweb = "Average", PCA1 = round(mean(dd_PCA$PCA1), 2), PCA2 = round(mean(dd_PCA$PCA2), 2),
                                   PCA3 = round(mean(dd_PCA$PCA3), 2), conn_cor = round(mean(abs(dd_PCA$conn_cor)),2)
                                   ))

names(dd_PCA)[1] <- "Food web"
dd_PCA$`Food web`[2] <- "Broadstone Stream (taxonomic aggregation)"
dd_PCA$`Food web`[12] <- "Broadstone Stream (size aggregation)"
names(dd_PCA)[2] <- "Principal \n Component I"
names(dd_PCA)[3] <- "Principal \n Component II"
names(dd_PCA)[4] <- "Principal \n Component III"
names(dd_PCA)[5] <- "Correlation(PCI, Connectance)"


# saveRDS(object = dd_PCA, file = "results/misc/PCA_properties.RDS")


