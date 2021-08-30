set.seed(78326)
library(foreach)
library(doSNOW)
res_rf <- readRDS("results/full_forest_all_2.RData")

thesevars <- c("neuroticism", "BIS", "balancedrelated", "extraversion", "externalizing", 
               "agreeableness", "leeftijd_moeder_11", "conflict_parents", "IRI_ped_aa", 
               "CRSI_en_ab", "selfconc", "CRSI_ps_av", "CRSI_en_am", "conflictfrequency", 
               "anxiety", "peermanag", "depression", "CRSI_wi_ab", "CRSI_wi_am", 
               "CRSI_en_av", "dailyhassles", "leeftijd_vader_11", "CRSI_ps_am", 
               "IRI_fan_aa", "intrusiveness", "leeftijd_target_11", "conpsy", 
               "conflictemo", "DMD_guilt", "externalizing_psych", "geslacht", 
               "tolerance", "supportivecriticism", "negativeaffect_father", 
               "openness", "IRI_pet_aa", "drugs", "DMD_tired", "power_father", 
               "DMD_angry", "CRSI_co_av", "pubert", "emotionalresponse", "IRI_emp_aa"
) #names(VI$variable.importance)[order(VI$variable.importance, decreasing = T)][1:44]
numvars <- thesevars[!sapply(res_rf$data[thesevars], inherits, what = "factor")]
probs <- pnorm(c(-1, 0, 1))
param.id <- c("meani", "means", "meanq")

for(thisvar in numvars){
  xgrid <- quantile(res_rf$data[[thisvar]], probs = probs)
  fd <- semtree:::partialDependenceDataset(res_rf$data, thisvar, xgrid)
  mapreduce <- function(tree) {
    t(sapply(1:nrow(fd), function(i){
      thisrow <- fd[i, , drop = FALSE]
      leaf.id <- semtree:::traverse(tree, thisrow)
      unname(unlist(c(thisrow[thisvar], semtree:::getNodeById(tree, leaf.id)$params[param.id])))
    }))
  }
  mapresult <- vector("list", length = length(res_rf$forest))
  chunks <- cut(1:length(res_rf$forest), 100)
  for(thechunk in levels(chunks)[-1]){
    i = 1
    while(i < 20){
      tmp <- try(foreach(i = which(chunks == thechunk), .packages = c("semtree")) %dopar% {
        mapreduce(res_rf$forest[[i]])
      })
      if(!inherits(tmp, "try-error")) break
      stopCluster(cl)
      rm(cl)
      cl<-makeCluster(4) #change the 2 to your number of CPU cores
      registerDoSNOW(cl)
      i <- i + 1
    }
    if(!inherits(res_rf, "try-error")) mapresult[which(chunks == thechunk)] <- tmp
  }
}

cl<-makeCluster(10) #change the 2 to your number of CPU cores
registerDoSNOW(cl)
pdps[[1]] <- plot_pdp(res_rf, "neuroticism")
stopCluster(cl)
rm(cl)
plot_pdp <- function (x,
                      variable,
                      growth_pars = c("meani", "means", "meanq"),
                      times = matrix(c(rep(1, 5), 0:4, (0:4)^2), nrow = 5),
                      probs = pnorm(c(-1, 0, 1))){
  #isfac <- inherits(x$data[[variable]], "factor")

# Partial dependence ------------------------------------------------------
  if (!variable %in% names(x$data)) {
    stop("Reference variable is not in the dataset")
  }
  model <- x$model
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    model.params <- names(OpenMx::omxGetParameters(x$model))
  }
  else if (inherits(model, "lavaan")) {
    if (is.null(x$forest[[1]])) {
      stop("Error! First tree is NULL")
    }
    model.params <- x$forest[[1]]$param_names
  }
  else {
    stop("Not supported!")
  }
  if (!all(growth_pars %in% model.params)) {
    stop("Some growth_pars are not in the model")
  }
  param.id <- match(growth_pars, model.params)
  refVar <- x$data[, variable]
  isfac <- inherits(refVar, "factor")
  if (isfac) {
    xgrid <- levels(refVar)
  }
  else {
    xgrid <- quantile(refVar, probs = probs)
  }
  fd <- semtree:::partialDependenceDataset(x$data, variable, 
                                 xgrid)
  mapreduce <- function(tree) {
    leaf.ids <- semtree:::traverse(tree, fd)
    ret <- vector("list", length(leaf.ids))
    for (j in 1:length(leaf.ids)) {
      node <- semtree:::getNodeById(tree, leaf.ids[j])
      p.estimate <- node$params[param.id]
      yvalue <- fd[j, variable]
      ret[[j]] <- (list(key = as.character(yvalue), value = p.estimate))
    }
    return(ret)
  }
  mapresult <- vector("list", length = length(x$forest))
  chunks <- cut(1:length(x$forest), 100)
  for(thechunk in levels(chunks)[-1]){
    i = 1
    while(i < 20){
      tmp <- try(foreach(i = which(chunks == thechunk), .packages = c("semtree")) %dopar% {
        mapreduce(x$forest[[i]])
      })
      if(!inherits(tmp, "try-error")) break
      stopCluster(cl)
      rm(cl)
      cl<-makeCluster(10) #change the 2 to your number of CPU cores
      registerDoSNOW(cl)
      i <- i + 1
    }
    if(!inherits(res_rf, "try-error")) mapresult[which(chunks == thechunk)] <- tmp
  }
  
  # mapresult <- foreach(i = 1:length(x$forest), .packages = c("semtree")) %dopar% {
  #   mapreduce(x$forest[[i]])
  # }
  
    # future.apply::future_lapply(FUN = , 
    #                                        X = x$forest)
  mr <- matrix(unlist(mapresult), ncol = length(param.id)+1, byrow = TRUE)
  keys <- mr[,1, drop = TRUE]
  mr <- mr[, -1, drop = FALSE]
  class(mr) <- "numeric"
  df_plot <- do.call(rbind, lapply(unique(keys), function(k){
    meds <- robustbase::colMedians(mr[keys == k,])
    data.frame(x = k, y = rowSums(matrix(meds, nrow(times), ncol(times), byrow = TRUE) * times), Time = 1:nrow(times))
  }))

# Plot --------------------------------------------------------------------

  # df_plot <- data.frame(do.call(rbind, lapply(1:nrow(times), function(t){
  #   cbind(x = unique(keys), y = rowSums(mr * matrix(times[t,], dim(mr)[1], ncol(times), byrow = TRUE)), Time = t)
  # })))
  if(!isfac) df_plot$x <- ordered(df_plot$x, levels = unique(df_plot$x)[order(as.numeric(unique(df_plot$x)), decreasing = TRUE)], labels = formatC(as.numeric(unique(df_plot$x))[order(as.numeric(unique(df_plot$x)), decreasing = TRUE)], digits = 2, format = "f"))
  names(df_plot)[1] <- variable
  df_plot$facet <- variable
  p <- ggplot(df_plot, aes_string(x = "Time", y = "y", shape = variable, group = variable, linetype = variable)) +
    geom_path() +
    geom_point() +
    #labs(title = NULL, x = NULL, y = NULL) +
    theme_bw() +
    facet_wrap(~facet)
  # if(!isfac){
  #   p <- p + theme(legend.position = "none")
  # } else {
  #   p + theme(legend.position = c(.9, .5))
  # }
  p
}
# list
# <- plot_pdp(tmp, "neuroticism")
# pneur +
#   #scale_y_continuous(limits = range(df_anal[grep("^de[2-6]$", names(df_anal), value = TRUE)]))
#   scale_y_continuous(quantile(unlist(df_anal[grep("^de[2-6]$", names(df_anal), value = TRUE)]), c(.025, .975)))
# pgesl <- plot_pdp(tmp, "geslacht")
# 
# pgesl +
#   scale_linetype_discrete(labels = c("Boy", "Girl")) +
#   scale_shape_discrete(labels = c("Boy", "Girl")) +
#   theme(legend.title = element_blank(), legend.position = c(.9, .5)) +
#   scale_y_continuous(limits = quantile(unlist(df_anal[grep("^de[2-6]$", names(df_anal), value = TRUE)]), c(.05, .95)))
# quantile(unlist(df_anal[grep("^de[2-6]$", names(df_anal), value = TRUE)]), c(.025, .975))
#  <- plot_pdp(tmp, "neuroticism")
