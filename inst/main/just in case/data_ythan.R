# 28.09.2021
# We download the Ythan food web dataset and construct the predation matrix

ythan_spnames <- read.csv("data/small_reef/ythan_spnames.csv")

n_species <- dim(ythan_spnames)[1]
pred_mat <- matrix(data = 0, nrow = n_species, ncol = n_species)
rownames(pred_mat) <- rev(ythan_spnames[,1])
colnames(pred_mat) <- rev(ythan_spnames[,1])

pred_mat <- ythan_spnames[, 2:93]
pred_mat <- apply(pred_mat, 2, rev)
pred_mat <- apply(pred_mat, 1, rev)

pred_mat <- t(pred_mat)
