par_forest <- function (model, data, control = NULL, predictors = NULL, constraints = NULL, 
                        ...) 
{
  arguments <- list(...)
  debugtree <- NULL
  covariates <- predictors
  if ((!is.null(arguments)) & ("covariates" %in% names(arguments))) {
    if (is.null(predictors)) {
      covariates <- arguments$covariates
    }
    else {
      stop("Cannot have predictors and covariates in SEM Tree model.")
    }
  }
  if ("semforest.control" %in% names(arguments)) {
    control <- arguments$semforest.control
    warning("Warning! Deprecated use of semforest.control!")
  }
  if ("seeds" %in% names(arguments)) {
    seeds <- arguments$seeds
  }
  else {
    seeds <- NULL
  }
  if ("with.error.handler" %in% names(arguments)) {
    with.error.handler <- arguments$with.error.handle
  }
  else {
    with.error.handler <- TRUE
  }
  result <- list()
  class(result) <- "semforest"
  result$param.names <- semtree:::getOCParameterNames(model, data)
  if (is.null(covariates)) {
    covariates <- result$param.names$covariates
    covariate.ids <- sapply(covariates, function(cv) {
      which(cv == names(data))
    })
  }
  else {
    covariate.ids <- sapply(covariates, function(cv) {
      which(cv == names(data))
    })
  }
  if (is.null(control)) {
    control <- semtree::semforest.control()
    ui_message("Default SEM forest settings established since no semforest.controls provided.")
  }
  else {
    if (semtree:::checkControl(control) != TRUE) {
      stop("Unknown options in semforest.control object!")
    }
  }
  semforest.control <- control
  if (!is.na(semforest.control$semtree.control$seed)) {
    stop(paste("Error! semtree.control object inside semforest.control has a seed.\n", 
               "Instead, use seed argument of semforest() function to specify seeds for reproducible analysis!"))
  }
  if (!checkControl(semforest.control$semtree.control)) {
    ui_stop("Unknown options in semforest.control$semtree.control object!")
  }
  if (!is.na(semforest.control$semtree.control$mtry)) {
    ui_stop("mtry manualy set in  semforest.control$semtree.control object! Please set mtry in semforest.control object only!")
  }
  semforest.control$semtree.control$mtry <- semforest.control$mtry
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    if (!summary(model)$wasRun) {
      ui_message("Model was not run. Estimating parameters now before running the forest.")
      model <- OpenMx::mxTryHard(model)
    }
  }
  forest.data <- list()
  forest.data <- replicate(semforest.control$num.trees, semtree:::forest.sample(data, 
                                                                      mtry = semforest.control$premtry, covariates, return.oob = T, 
                                                                      type = semforest.control$sampling), simplify = F)
  if (is.null(seeds)) {
    seeds <- rep(NA, semforest.control$num.trees)
  }
  else if (length(seeds) == 1 && seeds == TRUE) {
    seeds <- runif(n = semforest.control$num.trees, max = .Machine$integer.max)
  }
  else {
    if (length(seeds) != semforest.control$num.trees) {
      ui_stop("Number of seeds given does not match number of trees!")
    }
  }
  if (!is.null(debugtree)) {
    skip <- rep(TRUE, semforest.control$num.trees)
    skip[debugtree] <- FALSE
  }
  else {
    skip <- rep(FALSE, semforest.control$num.trees)
  }
  start.time <- proc.time()
  browser()
  trees <- future.apply::future_mapply(FUN = semtree:::semtreeApplyWrapper, 
                                       forest.data, seeds, skip, MoreArgs = list(model = model, 
                                                                                 semtree.control = semforest.control$semtree.control, 
                                                                                 with.error.handler, predictors = covariates, constraints = constraints), 
                                       SIMPLIFY = FALSE, future.seed = TRUE)
  for (i in 1:length(trees)) {
    if (!is.null(trees[[i]])) {
      trees[[i]]$name <- paste0("Tree #", i)
    }
  }
  elapsed <- proc.time() - start.time
  trees <- lapply(X = trees, FUN = postprocess)
  result$covariates <- covariates
  result$data <- data
  result$model <- model
  result$forest.data <- forest.data
  result$forest <- trees
  result$control <- semforest.control
  result$constraints <- constraints
  result$elapsed <- elapsed
  result$seeds <- seeds
  ui_ok("Forest completed [took ", human_readable_time(elapsed[3]), 
        "]")
  return(result)
}
