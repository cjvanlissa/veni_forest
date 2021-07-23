### Emotional Dysregulation # Assessing SEM-forest results ###

#############################
# Lukas Beinhauer
# 27/06/20
#############################

########################################################################################################
# Over the following lines, code can be found to assess the results from the SEM-forest                #
# Important: Run <missRanger.R>, <creatingscales.R > and < plantingforest.R > first!                                  #
########################################################################################################

forestdata <- readRDS("data/forestfile.RData")

library(semtree)
library(metaforest)


# system.time(# time of growing forest consisting of 10 trees: 238.52s; forest with 50 trees 1167.93s
#   vim <- varimp(predgrowthforest)
# ) #assessing variabel importance of that forest: 93.4 s; forest w/ 50 trees: 343.36s

system.time(
  vim <- varimp(forestdata)
)

plot(vim) 
#result from 50 trees, top 5:
#   1.: ra11aa: 14.666
#   2.: as11aa: 14.122
#   3.: ys11aa: 11.441
#   4.: sc11aa: 10.262
#   5.: pp11av: 9.700

print(vim)
# vim$importance

# forestdata$forest[[1]]$param_names

saveRDS(vim, "data/forestvim.RData")

