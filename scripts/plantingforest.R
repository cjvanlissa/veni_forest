### Emotional Dysregulation # SEM-trees ###

########################################################################################################
# Over the following lines, code can be found in order grow SEM-trees or a SEM-forest                  #
# Important: Run  <creatingscales.R > first!                                                           #
########################################################################################################
library(worcs)
library(semtree)
library(semPlot)
library(lavaan)
library(tidySEM)
library(semtree)
library(metaforest)
library(future.apply)
library(yaml)

df_anal <- load_data(to_envir = FALSE)$df_anal

dich_vars <- c("cigarettes", "alcohol", "drugs")
df_anal[dich_vars] <- lapply(df_anal[dich_vars], function(i){as.factor(!(i == min(i, na.rm = TRUE)))})

facs <- c("geslacht", "brpmoe_lmh", "brpvad_lmh", "sesgez_low", "bg11aa04", "reli")
df_anal[facs] <- lapply(df_anal[facs], as.factor)

# Specifying model linear growth curve model using the openMX code (should be the same model as the lavaan model)

dersvar <- c(paste0("de", 2:6))
predvar <- names(df_anal)[!names(df_anal) %in% dersvar]
df_anal <- as.data.frame(df_anal)
df_anal <- df_anal[, !names(df_anal) %in% c("awareness", "clarity", "impulsivity", 
                        "goals", "accept", "strategies")]

# Preliminary variable selection ------------------------------------------

library(psych)
fa.parallel(df_anal[, grepl("^de[2-6]", names(df_anal))])
dv <- principal(df_anal[, grepl("^de[2-6]", names(df_anal))])
dv$scores
df_pres <- data.frame(dv$scores, df_anal[, !grepl("^de[2-6]", names(df_anal))])
library(ranger)
library(tuneRanger)
tunetask <- makeRegrTask(data = df_pres, target = "PC1")
res_tune <- tuneRanger(tunetask, num.trees = 1000, 
                 num.threads = 40, iters = 70, save.file.path = NULL)
res_tune$model$learner.model

set.seed(57)
res <- ranger(PC1~., data = df_pres, importance = "impurity_corrected", mtry = 35, min.node.size = 4, num.trees = 1000)
VI <- ranger::importance_pvalues(res, method = "altmann", formula = PC1~., data = df_pres)
sum(VI[,2] < .05)
lastsig <- order(VI[,1], decreasing = TRUE)
lastsig <- max(which(VI[lastsig, 2] < .05))
selected <- rownames(VI)[VI[, 2] < .05]
p <- VarImpPlot(res, lastsig)
p <- p + 
  geom_point(data = data.frame(p$data, sig = (VI[,2] < .05)[as.character(p$data$Variable)]), aes(fill = sig), shape = 21) + 
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"))+
  theme(legend.position = "none")


# SEM forest --------------------------------------------------------------
basicgrowth <- lavaan::growth("i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6
q =~ 0*de2 + 1*de3 + 4*de4 + 9*de5 + 16*de6" , data = df_anal)
write_yaml(as.list(fitmeasures(basicgrowth)), "fitmeasures_growth.yml")
m0 <- as_ram("i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6
q =~ 0*de2 + 1*de3 + 4*de4 + 9*de5 + 16*de6
i ~ meani*1
s ~ means*1
q ~ meanq*1
de2 ~~ vres*de2
de3 ~~ vres*de3
de4 ~~ vres*de4
de5 ~~ vres*de5
de6 ~~ vres*de6
de2 ~ 0*1
de3 ~ 0*1
de4 ~ 0*1
de5 ~ 0*1
de6 ~ 0*1
i ~~ 0*s
i ~~ 0*q
s ~~ 0*q
i ~~ 0*i
s ~~ 0*s
q ~~ 0*q")
m0 <- run_mx(m0, data = df_anal)
table_results(m0)
table_fit(m0)
# fitting a single tree using the model

# system.time(
#   predgrowthtree <- semtree(m0, data = df_anal, control = semtree.control())
# ) # 856.14

#cl <- makeCluster(40)  # cluster of 2 CPUs created, in parallel
# A cluster can be stopped when not needed.
# Note – if you want to restart the cluster,
# you must use the sfClusterEval functions to load the packages for use with the new cluster.
# 
# stopCluster(cl)
# A SEM Forest can be grown in parallel by specifying the cluster to be used:
#   


####################### Tuning parameters for semForest algorithm ################################

# setting some controls, current: default method, 1000 trees in forest
controls <- semforest.control()
controls$num.trees <- 10 # number of trees to grow
controls$sampling <- "bootstrap" # number of trees to grow
controls$mtry <- floor(sqrt(length(predvar)))
controls$semtree.control
controls$semtree.control$alpha <- 0.05
controls$semtree.control$min.N <- 50
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
controls

mxOption(model= NULL, key="Number of Threads", value=1)
plan(multisession, workers = 10)
res_rf <- par_forest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls)
# Change the Default settings in semforest.control() and semtree.control()
# set.seed(78326)
# cl<-makeCluster(10) #change the 2 to your number of CPU cores
for(reps in 1:100){
  i = 1
  while(i < 20){
    res_rf <- try(semtree::semforest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls))
    if(!inherits(res_rf, "try-error")) break
    plan(multisession, workers = 10)
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_", reps, "_", Sys.time(), ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
f <- lapply(f, readRDS)
#dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
res_rf <- f[[1]]#readRDS(f[which.max(dts)])
for(i in f[-1]){
  out <- try({merge(res_rf, i)})
  if(!inherits(out, "try-error")){
    res_rf <- out
  } else {
    cat("File ", i, " could not be merged.")
  }
}
nullforests <- sapply(res_rf$forest, is.null)
res_rf$forest <- res_rf$forest[!nullforests]
saveRDS(res_rf, "results/full_forest.RData")
res_rf <- readRDS("results/full_forest.RData")
# vim <- varimp(res_rf)
library(future)
plan(multisession, workers = 40)
vim <- semtree::varimp(res_rf)
saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(VI) <- "ranger"
metaforest::VarImpPlot(VI)

# test <- semtree(m0, predictors=predvar[sample.int(length(predvar), 3)], control = controls)
# # plot(predgrowthtree) # tree can be plotted
# # summary(predgrowthtree)
# 
# # RandomForest 
# # may take a long time (previously on single core: 10 trees = 2.5 min, 100 trees = 23 min, 1000 trees = 3.5 hours)
# system.time(
#   predgrowthforest <- semforest(m0, data=df_anal, predictors=predvar, control=controls)
# )
# # summary(predgrowthforest)
# 
# 
# 
# saveRDS(predgrowthforest, "data/forestfile.RData") 


# Examine results ---------------------------------------------------------

M_dist <- semtree::proximity(res_rf)



# With full dataset -------------------------------------------------------

# setting some controls, current: default method, 1000 trees in forest
controls <- semforest.control()
controls$num.trees <- 10 # number of trees to grow
controls$sampling <- "bootstrap" # number of trees to grow
controls$mtry <- floor(sqrt(length(predvar)))
controls$semtree.control
controls$semtree.control$alpha <- 0.05
controls$semtree.control$min.N <- 50
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
controls

mxOption(model= NULL, key="Number of Threads", value=1)
plan(multisession, workers = 10)

for(reps in 1:100){
  i = 1
  while(i < 20){
    res_rf <- try(semtree::semforest(m0, data = df_anal, control = controls))
    if(!inherits(res_rf, "try-error")) break
    plan(multisession, workers = 10)
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_all_", reps, "_", Sys.time(), ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
f <- lapply(f, readRDS)
#dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
res_rf <- f[[1]]#readRDS(f[which.max(dts)])
for(i in f[-1]){
  out <- try({merge(res_rf, i)})
  if(!inherits(out, "try-error")){
    res_rf <- out
  } else {
    cat("File ", i, " could not be merged.")
  }
}
nullforests <- sapply(res_rf$forest, is.null)
res_rf$forest <- res_rf$forest[!nullforests]
saveRDS(res_rf, "results/full_forest.RData")
res_rf <- readRDS("results/full_forest.RData")
# vim <- varimp(res_rf)
library(future)
plan(multisession, workers = 40)
vim <- semtree::varimp(res_rf)
saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(VI) <- "ranger"
metaforest::VarImpPlot(VI)
