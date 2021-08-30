### Emotional Dysregulation # SEM-trees ###

########################################################################################################
# Over the following lines, code can be found in order grow SEM-trees or a SEM-forest                  #
# Important: Run  <creatingscales.R > first!                                                           #
########################################################################################################
library(worcs)
library(semtree)
library(lavaan)
library(tidySEM)
library(metaforest)
library(future.apply)

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

# SEM forest --------------------------------------------------------------

m0 <- growth("i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
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
q ~~ 0*q", data = df_anal)

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
#controls$semtree.control$min.bucket <- 10
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
#controls$semtree.control$verbose <- TRUE
controls

#plan(sequential)
set.seed(2145)

for(reps in 1:100){
  i = 1
  while(i < 20){
    res_rf <- try(semtree::semforest(m0, data = df_anal, control = controls))
    if(!inherits(res_rf, "try-error")) break
    #plan(multisession, workers = 10)
    i <- i + 1
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_lav_", reps, "_", Sys.time(), ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files(pattern = "^forest_lav.+?\\.RData$", full.names = T)
f <- lapply(f, readRDS)
#dts <- as.Date(gsub("^forest_all_\\d{1,}_2021-08-21 1[345].+?\\.RData", "\\1", f))
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
res_rf$forest[[3]]$left_child$
trunk_only <- sapply(res_rf$forest, function(x){is.null(x[["left_child"]])})
res_rf$forest <- res_rf$forest[!trunk_only]
saveRDS(res_rf, "results/full_forest_lav.RData")
