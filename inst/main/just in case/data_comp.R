# 28.09.2021
# Compare the dataset


####### Benguela Pelagic

fw_name <- "Benguela Pelagic"
fw_2008 <- readRDS(paste0("../C1_method/data_new/", fw_name, ".web.Rdata"))
fw_2021 <- readRDS(paste0("../C1_method_v2/data/from Owen/brose_2008_data_treatment/brose2005ecology_adbm2008/extracted_foodwebs/", fw_name, ".web.RDS"))

sum(fw_2008$species.names != fw_2021$species.names)
sum(fw_2008$predation.matrix != fw_2021$predation.matrix)



######## Broadstone Stream

fw_name <- "Broadstone Stream"
fw_2008 <- readRDS(paste0("../C1_method/data_new/", fw_name, ".web.Rdata"))
fw_2021 <- readRDS(paste0("../C1_method_v2/data/from Owen/brose_2008_data_treatment/brose2005ecology_adbm2008/extracted_foodwebs/", fw_name, ".web.RDS"))

sum(fw_2008$species.names != fw_2021$species.names)
sum(fw_2008$predation.matrix != fw_2021$predation.matrix)




