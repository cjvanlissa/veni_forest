# Prepare matrices for shiny ap -------------------------------------------
f <- c("scale_rename.csv", "desc_cat.csv", "desc_num.csv", "classplot_color.RData", "results/forest_light.RData", "shiny_num_mat.csv")
sapply(f, function(i){
  file.copy(i, to = file.path("c:/git_repositories/shiny_emoreg", basename(i)), overwrite = TRUE)
})

res_rf <- readRDS("results/full_forest_all_2.RData")
df <- res_rf$data
rm(res_rf)

tmp <- df[, !names(df) %in% c("geslacht", "brpmoe_lmh", "brpvad_lmh", "sesgez_low", "bg11aa04", "reli", "cigarettes", "alcohol", "drugs")]
num_mat <- sapply(tmp, quantile, probs = pnorm(c(-2, -1, 0, 1, 2)))
num_mat <- data.frame(name = colnames(num_mat), t(num_mat))
renm <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = F)
#num_mat$name <- renm$V2[match(num_mat$name, renm$V1)]
num_mat <- num_mat[!is.na(num_mat$name), ]
vim <- readRDS("variable_importance.RData")
num_mat <- num_mat[order(vim[num_mat$name], decreasing = TRUE), ]
names(num_mat)[-1] <- paste0("Q", c(1:5))
write.csv(num_mat, "shiny_num_mat.csv", row.names = FALSE)