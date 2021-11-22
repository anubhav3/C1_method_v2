# 28.09.2021
# We download the Small Reef food web dataset and construct the predation matrix

library(readxl)
full_data <- read.csv("~/Google Drive/GitHub/C1_method_v2/data/secondary/small_reef/full_data.csv", sep=";")
reef_spnames <- read_excel("data/secondary/small_reef/reef_spnames.xls")

sr <- full_data %>%
  filter(Network == "reef")

N <- dim(reef_spnames)[1]
raw_pred_mat <- as.matrix(reef_spnames[,2:(N+1)])
rownames(raw_pred_mat) <- colnames(raw_pred_mat)

sr$BodyWeight <- as.numeric(sr$BodyWeight)

sr <- sr %>%
  arrange(BodyWeight) %>%
  filter(!is.na(BodyWeight))

pred_mat <- raw_pred_mat[sr$Species, sr$Species]


all.web.info_small_reef <- list(web.name="Small Reef",
                                species.names=rownames(pred_mat),
                                species.sizes=sr$BodyWeight,
                                species.met.cat=NA,
                                feeding.interaction=NA,
                                species.abundance=NA,
                                predation.matrix=pred_mat)

saveRDS(all.web.info_small_reef, file=paste("data/", "Small Reef", ".web.RDS", sep=""))

