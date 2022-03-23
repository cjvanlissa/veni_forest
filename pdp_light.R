library(ggplot2)
library(gtable)
library(grid)
f <- readRDS('results/forest_light.RData')
class(f) <- c("semforest_light", "list")
attr(f, "parameters") <- c("meani", "means", "meanq")
dat <- readRDS("data.RData")
renm <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = FALSE)
vim <- readRDS('variable_importance.RData')
catlevs <- read.csv("desc_cat.csv", stringsAsFactors = FALSE)
times <- matrix(c(rep(1, 5), 0:4, c(0:4)^2), nrow = 5)
lvls <- c("-1SD", "mean", "+1SD")
for(i in 1:length(vim)){
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
  saveRDS(tmp, file = paste0("pdpdat_", v, ".RData"))
}


# Merge plots -------------------------------------------------------------
scls <- c(-.01, .06)
revcod <- readRDS('revcod.RData')
p_list <- vector("list", length = length(vim))
for(i in 1:length(vim)){
  n <- names(vim)[i]
  v <- renm$V1[pmatch(names(vim)[i], renm$V2)]
  tmp <- readRDS(file = paste0("pdpdat_", v, ".RData"))
  tmp$value <- -1 * tmp$value
  if("mean" %in% tmp[[1]]){
    if(n %in% revcod){
      levels(tmp[[1]]) <- levels(tmp[[1]])[c(3,2,1)]
    }
    tmp[[1]] <- ordered(tmp[[1]], levels = c("+1SD", "mean", "-1SD"))
  }
  tmp$Variable <- paste0(i, ". ", names(vim)[i])
  p_list[[i]] <- ggplot(tmp, aes_string(x = "Time", y = "value", linetype = names(tmp)[1], shape = names(tmp)[1])) +
    geom_path() +
    geom_point() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "none",
          legend.key = element_blank(), legend.background=element_blank()) +
    scale_y_continuous(limits = scls) +
    facet_wrap(~Variable) +
    scale_x_continuous(breaks = 1:5, labels = 14:18)
}

chunks <- list(
  1:20, 21:40, 41:60, 61:87
)
for(thischunk in chunks){
  pdps <- p_list[thischunk]
  ylab <- "Emotion Regulation"
  catvars <- which(names(vim[thischunk]) %in% catlevs$name)
  n_grobs <- length(pdps)
  grob_rows <- ceiling(n_grobs/4)
  grob_cols <- 4
  for (x in 1:length(pdps)) {
    if (!(x %in% seq.int(1, n_grobs, by = grob_cols))) {
      pdps[[x]] <- pdps[[x]] + theme(axis.text.y = element_blank(), 
                                     axis.ticks.y = element_blank()) +
        labs(y = NULL)
    }
    if(!x %in% c(((n_grobs-grob_cols)+1):n_grobs)) {
      pdps[[x]] <- pdps[[x]] + theme(axis.text.x = element_blank(), 
                                     axis.ticks.x = element_blank()) +
        labs(y = NULL)
    }
    if(x == 12) {
      pdps[[x]] <- pdps[[x]] + theme(legend.position = c(.75, .2))
    }
    if(x %in% catvars){
      pdps[[x]] <- pdps[[x]] + theme(legend.position = c(.6, .35))
    }
    if(x == 12 & 72 %in% thischunk){
      pdps[[x]] <- pdps[[x]] + theme(legend.position = c(.75, .3))
    }
    pdps[[x]] <- suppressMessages(ggplotGrob(pdps[[x]] + 
                                               theme(axis.title.y = element_blank(),
                                                     axis.title.x = element_blank())))
    if (x > 1) 
      pdps[[x]]$widths <- pdps[[1]]$widths
  }
  if (n_grobs < (grob_cols * grob_rows)) {
    pdps[(length(pdps) + 1):(grob_cols * grob_rows)] <- lapply((length(pdps) + 
                                                                  1):(grob_cols * grob_rows), function(x) {
                                                                    nullGrob()
                                                                  })
  }
  gt <- gtable_matrix("partial.dependence", matrix(pdps, 
                                                   nrow = grob_rows, byrow = TRUE), widths = unit(rep(1, 
                                                                                                      grob_cols), "null"), heights = unit(rep(1, grob_rows), 
                                                                                                                                          "null"))
  left <- textGrob(ylab, rot = 90, just = c(0.5, 0.5))
  gt <- gtable_add_cols(gt, widths = grobWidth(left) + unit(0.5, 
                                                            "line"), 0)
  gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt), l = 1, 
                        r = 1, z = Inf)
  gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))
  png(file.path("plots", paste0("pdp", thischunk[1], "-", tail(thischunk, 1), ".png")), height = 297, width = 210, units = "mm", res= 300)
  grid.newpage()
  grid.draw(gt)
  dev.off()
  
  
  postscript(file.path("plots", paste0("pdp", thischunk[1], "-", tail(thischunk, 1), ".eps")), height = 11.7, width = 8.3,
             family = "sans", paper = "special", onefile = FALSE,
             horizontal = FALSE)
  grid.newpage()
  grid.draw(gt)
  dev.off()
  
  svg(file.path("plots", paste0("pdp", thischunk[1], "-", tail(thischunk, 1), ".svg")), height = 11.7, width = 8.3)
  grid.newpage()
  grid.draw(gt)
  dev.off()
  
}
