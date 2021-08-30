library(gtable)
library(grid)
library(ggplot2)


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
# library(future)
# res_rf <- readRDS("results/full_forest_all_2.RData")
# plan(multisession, workers = 4, gc = TRUE)
# for(thisvar in numvars){
#   pdps <- try(plot_pdp(res_rf, thisvar))
#   if(!inherits(pdps, "try-error")) saveRDS(pdps, file.path("pdp", paste0(thisvar, ".RData")))
# }  
# numvars <- thesevars[!sapply(res_rf$data[thesevars], inherits, what = "factor")]
# catvars <- thesevars[sapply(res_rf$data[thesevars], inherits, what = "factor")]
catvars <- c("geslacht", "drugs")
relab <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = FALSE)
relab <- relab[match(relab$V1, thesevars), ]
relab$V2 <- paste0(1:nrow(relab), ". ", relab$V2)
relabels <- relab$V2
names(relabels) <- relab$V1
f <- file.path("pdp", paste0(thesevars, ".RData"))
all(file.exists(f))
chunks = list(1:12, 13:24, 25:36, 37:44)
for(thischunk in chunks){
  thisvars <- thesevars[thischunk]
  pdps <- lapply(file.path("pdp", paste0(thisvars, ".RData")), readRDS)
  pdps <- lapply(pdps, function(p){p + scale_y_continuous(limits = c(-0.07, 0.01)) })
  pdps <- lapply(pdps, function(p){
    p$data$variable <- relabels[p$data$variable]
    p + facet_wrap(~variable)
  })
  ylab <- "Difficulties in Emotion Regulation"
  catvars <- which(thisvars %in% c("drugs", "geslacht"))
  n_grobs <- length(pdps)
  grob_rows <- ceiling(n_grobs/3)
  grob_cols <- 3
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
    if(!(x == grob_cols | x %in% catvars)) {
      pdps[[x]] <- pdps[[x]] + theme(legend.position = "none")
    } else {
      pdps[[x]] <- pdps[[x]] + theme(legend.position = c(.7, .7), legend.title = element_blank())
      if(!x %in% catvars){
        pdps[[x]] <- pdps[[x]] +
          scale_linetype_discrete(labels = c("+1SD", "mean", "-1SD")) +
          scale_shape_discrete(labels = c("+1SD", "mean", "-1SD"))
      } else {
        thelabs <- list(
          geslacht = c("1" = "Boy", "2" = "Girl"),
          drugs = c("TRUE" = "Yes", "FALSE" = "No")
        )
        thelabs <- thelabs[[thisvars[x]]]
        pdps[[x]]$data[[1]] <- thelabs[pdps[[x]]$data[[1]]]
      }
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