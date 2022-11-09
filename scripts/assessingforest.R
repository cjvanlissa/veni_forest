library(semtree)
library(metaforest)
library(future)

res_rf <- readRDS("results/full_forest_all_2.RData")
fac_labs <- list(
  geslacht = c(Girl = 2,
               Boy = 1),
  brpmoe_lmh = c(High = 3,
                 Medium = 2, Low = 1),
  brpvad_lmh = c(High = 3, Medium = 2, Low = 1),
  sesgez_low = c(`Low` = 1, `Med/hi` = 0),
  bg11aa04 = c(
    `None` = 8,
    `Other` = 7,
    Buddhist = 6,
    Hindu = 5,
    Islam = 4,
    Reformed = 3,
    `Dutch ref.` = 2,
    `Catholic` = 1
  ),
  reli = c(Yes = 1,
           No = 0),
  alcohol = c("FALSE" = "No",
              "TRUE" = "Yes"),
  cigarettes = c("FALSE" = "No",
                 "TRUE" = "Yes"),
  drugs = c("FALSE" = "No",
            "TRUE" = "Yes")
)

#plan(multisession, workers = 40)
#vim <- semtree::varimp(res_rf)
#saveRDS(vim, paste0("results/vim_all_2", gsub("[: ]", "_", Sys.time()), ".RData"))
vim <- readRDS("results/vim_all_22021-08-21_17_23_27.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
ren <- read.csv("scale_rename.csv", stringsAsFactors = F, header = FALSE)
VI$variable.importance <- sort(VI$variable.importance, decreasing = TRUE)

# Legend
leg <- read.csv("supplemental_table_1.csv", stringsAsFactors = FALSE)
legtab <- data.frame(Predictor = ren$V2[match(names(VI$variable.importance)[1:30], ren$V1)], Description = leg$description.en[match(names(VI$variable.importance)[1:30], leg$variable.name)])
                                        
write.csv(legtab, "legtab.csv", row.names = FALSE)
names(VI$variable.importance)[names(VI$variable.importance) %in% ren$V1] <- ren$V2[match(names(VI$variable.importance)[names(VI$variable.importance) %in% ren$V1], ren$V1)]
saveRDS(VI$variable.importance, "variable_importance.RData")

names(VI$variable.importance) <- paste0(1:length(VI$variable.importance), ". ", c(rep("  ", 9), rep("", (length(VI$variable.importance)-9))), names(VI$variable.importance))
class(VI) <- "ranger"
v1 <- v2 <- VI
v1$variable.importance <- v1$variable.importance[1:44]
p1 <- metaforest::VarImpPlot(v1, 44)+theme(axis.text.y = element_text(hjust=0))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$variable.importance)))
v2$variable.importance <- v2$variable.importance[45:87]
p2 <- metaforest::VarImpPlot(v2, 43)+theme(axis.text.y = element_text(hjust=0))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$variable.importance)))
library("cowplot")
pcomb <- plot_grid(p1,p2,
          ncol = 2, nrow = 1)
ggsave("pcomb.pdf", pcomb, device = "pdf")
saveRDS(pcomb, "varimp_comb_21-08-2021.RData")
p <- metaforest::VarImpPlot(VI, length(VI$variable.importance))
saveRDS(p, "varimp_21-08-2021.RData")


# Varimp by type ----------------------------------------------------------

VI <- readRDS("variable_importance.RData")
names(VI)[names(VI) == "Psychological control"] <- "Psychological control (mf)"
ren <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = FALSE)
VI <- data.frame(Variable = paste0(1:length(VI), ". ", names(VI)), Importance = VI, 
                 Level = ren$V4[match(names(VI), ren$V2)],
                 Theme= ren$V5[match(names(VI), ren$V2)])
VI$Level <- ordered(VI$Level, levels = rev(c("Individual", "Microsystem", "Mesosystem", "Macrosystem")))
VI$Theme <- ordered(VI$Theme, levels = rev(c("Personality", "Int/ext", "Conflict", "Parenting", "Substance", 
                                         "Peers", "Demographics")))

#VI <- VI[order(VI$Level, VI$Importance, decreasing = TRUE), ]
VI$Variable <- ordered(VI$Variable, levels = rev(VI$Variable))
p1 <- 
  ggplot(VI[1:44, ], aes(x = Importance, y = Variable, shape = Level)) +
    geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), colour = "grey50", linetype = 2) + 
    geom_vline(xintercept = 0, colour = "grey50", 
               linetype = 1) +
    geom_point(size = 2) +
    xlab("Variable Importance (Permutation importance)") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(), axis.title.y = element_blank())+
    theme(axis.text.y = element_text(hjust=0))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$Importance))) +
    scale_shape_manual(breaks = c("Individual", "Microsystem"), values = c(16, 17), guide = "none")
  
  
p2 <- 
  ggplot(VI[45:nrow(VI), ], aes(x = Importance, y = Variable, shape = Level)) +
    geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), colour = "grey50", linetype = 2) + 
    geom_vline(xintercept = 0, colour = "grey50", 
               linetype = 1) +
    geom_point(size = 2) +
    xlab("Variable Importance (Permutation importance)") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())+
    theme(axis.text.y = element_text(hjust=0), legend.position = c(.72, .15))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$Importance))) +
    scale_shape_manual(breaks = c("Individual", "Microsystem", "Mesosystem", "Macrosystem"), values = c(16, 17, 18, 3))


library("cowplot")
pcomb <- plot_grid(p1,p2,
                   ncol = 2, nrow = 1)
ggsave("pcomb2.svg", pcomb, device = "svg", width = 210, height = 130, units = "mm")
ggsave("pcomb2.png", pcomb, device = "png", width = 210, height = 130, units = "mm")
ggsave("pcomb2.pdf", pcomb, device = "pdf")
saveRDS(pcomb, "varimp_comb2_03-08-2022.RData")
p <- metaforest::VarImpPlot(VI, length(VI$variable.importance))
saveRDS(p, "varimp_21-08-2021.RData")

library(ggplot2)
ggplot(VI, aes(x = Importance, y = Level)) + geom_jitter(height = .2)

set.seed(1)

p1 <- ggplot(VI, aes(y = Level, x = Importance)) +
  geom_violin(position = position_dodge(), scale = "width") +
  geom_jitter(width = 0, height = .1)+
  theme(legend.position = "none") +
  theme_bw() +
  theme(axis.title =  element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("violin.svg", p, device = "svg", width = 105, height = 60, units = "mm")

df_plot <- VI
df_plot$Type = "B. Level"
df_plot$Label <- df_plot$Level
df_plot2 <- VI[!is.na(VI$Theme), ]
df_plot2$Type = "A. Theme"
df_plot2$Label <- df_plot2$Theme
df_plot <- rbind(df_plot, df_plot2)
p = ggplot(df_plot, aes(y = Label, x = Importance)) +
  #geom_boxplot()+
  geom_violin(position = position_dodge(), scale = "width") +
  geom_jitter(width = 0, height = .1)+
  theme(legend.position = "none") +
  theme_bw() +
  theme(axis.title =  element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~Type, scales = "free_y")
ggsave("violin.svg", p, device = "svg", width = 210, height = 60, units = "mm")
ggsave("violin.png", p, device = "png", width = 210, height = 60, units = "mm")
saveRDS(p, "violin.RData")

pcomb <- plot_grid(p1,p2,
                   ncol = 2, nrow = 1)
ggsave("violin_comb.svg", pcomb, device = "svg", width = 210, height = 130, units = "mm")
ggsave("pcomb2.pdf", pcomb, device = "pdf")
saveRDS(pcomb, "varimp_comb2_03-08-2022.RData")
p <- metaforest::VarImpPlot(VI, length(VI$variable.importance))
saveRDS(p, "varimp_21-08-2021.RData")
# Partial dependence: too computationally intensive -----------------------
#plan(multisession, workers = 10, gc = TRUE)
source("pdp_growth.R")
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

pdps <- vector("list", length = 44)
pdps[[1]] <- plot_pdp(res_rf, "neuroticism")


# Clustering --------------------------------------------------------------
# klsym
# plan(multisession, workers = 10)
# M <- diversityMatrix(res_rf)
# 

# a grid will show the proximity between every pair of observations.
# The proximity represents the percentage of trees where the two observations
# appear in the same leaf node.
# So the higher the value, the closer the observations.
# You can then use this proximity measure as the similarity or distance metric
# in your favorite clustering technique.