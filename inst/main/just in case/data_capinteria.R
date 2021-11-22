# 28.09.2021
# We download the Capinteria food web dataset and construct the predation matrix

library(R.utils)
sourceDirectory("../C1_method/R")

link_all <- read.delim("~/Google Drive/GitHub/C1_method_v2/data/secondary/capinteria/CSMweb_Links.txt")
node_all <- read.delim("~/Google Drive/GitHub/C1_method_v2/data/secondary/capinteria/CSMweb_Nodes.txt")


## Arrange the species by body mass and remove NULLs
node_all <- node_all %>%
  arrange(BodySize.g.) %>%
  filter(!is.na(BodySize.g.))

species_bs <- node_all %>%
  group_by(SpeciesID) %>%
  summarise(SpeciesID = SpeciesID, BodySize.g. = mean(BodySize.g.)) %>%
  arrange(BodySize.g.)


n_species <- length(species_bs$SpeciesID)
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)

rownames(pred_mat) <- species_bs$SpeciesID
colnames(pred_mat) <- species_bs$SpeciesID

bs_data <- link_all %>%
  filter(ResourceSpeciesID %in% species_bs$SpeciesID & ConsumerSpeciesID %in% species_bs$SpeciesID)

nrow_data <- dim(bs_data)[1]


for(i in 1:nrow_data){
  
  row_data <- bs_data[i,]
  
  predator_name <- as.character(row_data$ConsumerSpeciesID)
  prey_name <- as.character(row_data$ResourceSpeciesID)
  
  pred_mat[prey_name, predator_name] <- 1
}


all.web.info_capinteria <- list(web.name="Capinteria",
                           species.names=NA,
                           species.sizes=species_bs$BodySize.g.,
                           species.met.cat=NA,
                           feeding.interaction=NA,
                           species.abundance=NA,
                           predation.matrix=pred_mat)

saveRDS(all.web.info_capinteria, file=paste("data/", "Capinteria", ".web.RDS", sep=""))


View(node_all %>%
  filter(!is.na(BodySize.g.)) %>%
  group_by(SpeciesID) %>%
  summarise(var = var(BodySize.g.), mean = mean(BodySize.g.), n = length(unique(BodySize.g.))))


node_all %>%
  filter(SpeciesID == 233)
