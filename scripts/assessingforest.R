library(semtree)
library(metaforest)
library(future)

res_rf <- readRDS("results/full_forest_all_2.RData")

plan(multisession, workers = 40)
vim <- semtree::varimp(res_rf)
saveRDS(vim, paste0("results/vim_all_2", gsub("[: ]", "_", Sys.time()), ".RData"))
#vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(VI) <- "ranger"
p <- metaforest::VarImpPlot(VI, 92) 
saveRDS(p, "varimp_21-08-2021.RData")


# Partial dependence: too computationally intensive -----------------------

# names(VI$variable.importance)[which.max(VI$variable.importance)]
# p <- semtree::partialDependence(res_rf, reference.var = "neuroticism", reference.param = "meani", support = 20)


# Clustering --------------------------------------------------------------
klsym
plan(multisession, workers = 40)
M <- diversityMatrix(res_rf)


# a grid will show the proximity between every pair of observations.
# The proximity represents the percentage of trees where the two observations
# appear in the same leaf node.
# So the higher the value, the closer the observations.
# You can then use this proximity measure as the similarity or distance metric
# in your favorite clustering technique.