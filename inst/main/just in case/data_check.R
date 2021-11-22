# 20.09.2021
# Check the food web data

library(dplyr)

bodysizes_2008 <- read.delim(file = url("https://figshare.com/ndownloader/files/5595854"))
bs_all <- bodysizes_2008


###### Benguela Pelagic
fw_name <- "Benguela Pelagic"

bs_data <- bs_all %>%
  filter(Geographic.location == "Africa, Benguela ecosystem")

# Replacing Benthic carnvores with Benthic carnivores
bs_data <- bs_data %>%
  mutate(Common.name.s..consumer = ifelse(Common.name.s..consumer == "Benthic carnvores",
                                          "Benthic carnivores", Common.name.s..consumer),
         Common.name.s..resource = ifelse(Common.name.s..resource == "Benthic carnvores",
                                          "Benthic carnivores", Common.name.s..resource)
  )

M_consumer <- bs_data %>%
  group_by(Common.name.s..consumer) %>%
  summarise(mean = mean(Mean.mass..g..consumer)) %>%
  dplyr::select(species_name = Common.name.s..consumer, mean)

M_resource <- bs_data %>%
  group_by(Common.name.s..resource) %>%
  summarise(mean = mean(Mean.mass..g..resource)) %>%
  dplyr::select(species_name = Common.name.s..resource, mean)


species <- union(unique(bs_data$Common.name.s..consumer), unique(bs_data$Common.name.s..resource))


M_species <- rbind(M_consumer, M_resource)
M_species <- unique(M_species)
M_species <- M_species[order(M_species$mean),]

n_species <- length(species)
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)

rownames(pred_mat) <- M_species$species_name
colnames(pred_mat) <- M_species$species_name

nrow_data <- dim(bs_data)[1]

for(i in 1:nrow_data){
  
  row_data <- bs_data[i,]
  
  predator_name <- row_data$Common.name.s..consumer
  prey_name <- row_data$Common.name.s..resource
  
  pred_mat[prey_name, predator_name] <- 1
}

fw_data <- list(species.names = M_species$species_name, species.sizes = M_species$mean, predation.matrix = pred_mat)
# saveRDS(object = fw_data, file = paste0("data/", fw_name, ".web.Rdata"))



###### Broadstone Stream

fw_name <- "Broadstone Stream"

bs_data <- bs_all %>%
  filter(Geographic.location == "Country: United Kingdom; UTM: 51.05'N, 0.03'E; Broadstone Stream in Sussex")

species <- union(unique(bs_data$Taxonomy.consumer), unique(bs_data$Taxonomy.resource))

M_consumer <- bs_data %>%
  group_by(Taxonomy.consumer) %>%
  summarise(mean = mean(Mean.mass..g..consumer)) %>%
  dplyr::select(species_name = Taxonomy.consumer, mean)

M_resource <- bs_data %>%
  group_by(Taxonomy.resource) %>%
  summarise(mean = mean(Mean.mass..g..resource)) %>%
  dplyr::select(species_name = Taxonomy.resource, mean)

M_species <- rbind(M_consumer, M_resource)
M_species <- unique(M_species)
M_species <- M_species[order(M_species$mean),]

n_species <- length(species)
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)

rownames(pred_mat) <- M_species$species_name
colnames(pred_mat) <- M_species$species_name

nrow_data <- dim(bs_data)[1]

for(i in 1:nrow_data){
  
  row_data <- bs_data[i,]
  
  predator_name <- row_data$Taxonomy.consumer
  prey_name <- row_data$Taxonomy.resource
  
  pred_mat[prey_name, predator_name] <- 1
}

fw_data <- list(species.names = M_species$species_name, species.sizes = M_species$mean, predation.matrix = pred_mat)
# saveRDS(object = fw_data, file = paste0("data/", fw_name, ".web.Rdata"))


###### Broom

fw_name <- "Broom"

bs_data <- bs_all %>%
  filter(Geographic.location == "Country: United Kingdom; UTM: 51.24'N, 0.34'W; Silwood Park, Berkshire")

species <- union(unique(bs_data$Taxonomy.consumer), unique(bs_data$Taxonomy.resource))

M_consumer <- bs_data %>%
  group_by(Taxonomy.consumer) %>%
  summarise(mean = mean(Mean.mass..g..consumer)) %>%
  dplyr::select(species_name = Taxonomy.consumer, mean)

M_resource <- bs_data %>%
  group_by(Taxonomy.resource) %>%
  summarise(mean = mean(Mean.mass..g..resource)) %>%
  dplyr::select(species_name = Taxonomy.resource, mean)

M_species <- rbind(M_consumer, M_resource)
M_species <- unique(M_species)
M_species <- M_species[order(M_species$mean),]

n_species <- length(species)
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)

rownames(pred_mat) <- M_species$species_name
colnames(pred_mat) <- M_species$species_name

nrow_data <- dim(bs_data)[1]

for(i in 1:nrow_data){
  
  row_data <- bs_data[i,]
  
  predator_name <- row_data$Taxonomy.consumer
  prey_name <- row_data$Taxonomy.resource
  
  pred_mat[prey_name, predator_name] <- 1
}

fw_data <- list(species.names = M_species$species_name, species.sizes = M_species$mean, predation.matrix = pred_mat)
# saveRDS(object = fw_data, file = paste0("data/", fw_name, ".web.Rdata"))



###### Caricaie Lakes

fw_name <- "Caricaie Lakes"

bs_data <- bs_all %>%
  filter(Geographic.location == "Country: Switzerland; Lake Neuch\xe2tel")

species <- union(unique(bs_data$Taxonomy.consumer), unique(bs_data$Taxonomy.resource))

M_consumer <- bs_data %>%
  group_by(Taxonomy.consumer) %>%
  summarise(mean = mean(Mean.mass..g..consumer)) %>%
  select(species_name = Taxonomy.consumer, mean)

M_resource <- bs_data %>%
  group_by(Taxonomy.resource) %>%
  summarise(mean = mean(Mean.mass..g..resource)) %>%
  select(species_name = Taxonomy.resource, mean)

M_species <- rbind(M_consumer, M_resource)
M_species <- unique(M_species)
M_species <- M_species[order(M_species$mean),]

n_species <- length(species)
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)

rownames(pred_mat) <- M_species$species_name
colnames(pred_mat) <- M_species$species_name

nrow_data <- dim(bs_data)[1]

for(i in 1:nrow_data){
  
  row_data <- bs_data[i,]
  
  predator_name <- row_data$Taxonomy.consumer
  prey_name <- row_data$Taxonomy.resource
  
  pred_mat[prey_name, predator_name] <- 1
}

fw_data <- list(species.names = M_species$species_name, species.sizes = M_species$mean, predation.matrix = pred_mat)
# saveRDS(object = fw_data, file = paste0("data/", fw_name, ".web.Rdata"))


