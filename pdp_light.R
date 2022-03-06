f <- readRDS('c:/git_repositories/veni_forest/results/forest_light.RData')
f_full <- readRDS('c:/git_repositories/veni_forest/results/full_forest_all_2.RData')
class(f) <- c("semforest_light", "list")
attr(f, "parameters") <- c("meani", "means", "meanq")
dat <- readRDS("c:/tmp/data.RData")
renm <- read.csv("c:/git_repositories/veni_forest/scale_rename.csv", stringsAsFactors = FALSE, header = FALSE)
vim <- readRDS('c:/git_repositories/veni_forest/variable_importance.RData')
catlevs <- read.csv("c:/git_repositories/veni_forest/desc_cat.csv", stringsAsFactors = FALSE)
vim
times <- 
lvls <- c("-1SD", "mean", "+1SD")
for(i in 30:length(vim)){
  v <- renm$V1[match(names(vim)[i], renm$V2)]
  lbl <- paste0(i, ". ", names(vim)[i])
  col <- dat[[v]]
  if(inherits(col, "numeric")){
    points <- quantile(dat[[v]], probs = pnorm(c(-1, 0, 1)))
    l <- lvls
    if(any(duplicated(points))){
      rem <- which(duplicated(points))
      points <- points[-rem]
      l <- l[-rem]
    }
    points <- list(a = points)
    names(points) <- v
  } else {
    l <- catlevs$Category[catlevs$name == names(vim)[i]]
    points <- list(a = levels(col))
    names(points) <- v
  }
  tmp <- pd_growth(x = f, data = dat, reference.var = v, points = points, times = times, FUN = "mean")
  tmp[[1]] <- factor(tmp[[1]], labels = l)
  # ggplot(tmp, aes_string(x = "Time", y = "value", linetype = names(tmp)[1], shape = names(tmp)[1])) + geom_path() + geom_point()
  saveRDS(tmp, file = paste0("pdpdat_", v, ".RData"))
}

times <- matrix(c(rep(1, 5), 0:4, c(0:4)^2), nrow = 5)
.trajectory(df_plot[1, -1], t(times))

pd_full <- semtree::partialDependence(f_full, "neuroticism", "means", 2)
