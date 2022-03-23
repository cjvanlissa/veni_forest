library(tidySEM)
library(worcs)
load_data()
df <- df_anal[1:5]
class(df) <- "data.frame"
names(df) <- paste0("de", 1:5)
df[] <- lapply(df, `*`, -1)
res <- mx_growth_mixture(model = "i =~ 1*de1 + 1*de2 + 1*de3 +1*de4 +1*de5
                           s =~ 0*de1 + 1*de2 + 2*de3 +3*de4 +4*de5
                           q =~ 0*de1 + 1*de2 + 4*de3 +9*de4 +16*de5
                           de1 ~~ vde1*de1
                           de2 ~~ vde2*de2
                           de3 ~~ vde3*de3
                           de4 ~~ vde4*de4
                           de5 ~~ vde5*de5
                           i ~~ 0*i
                           s ~~ 0*s
                           q ~~ 0*q",
                  classes = 1:6,
                  data = df,
                  auto.cov.lv.x = FALSE
                  )
saveRDS(res, "mixmodrev.RData")
class(res) <- c("mixture_list", class(res))
table_fit(res)
# blrts <- BLRT(res, replications = 4)
props <- apply(class_prob(res[[2]])$individual, 1, which.max)

library(MplusAutomation)
res_mp <- createMixtures(classes = 1:6,
                         filename_stem = "mix",
                         model_overall =
"i s q | de1@0 de2@1 de3@2 de4@3 de5@4;
i-q@0;",
rdata = df,
usevariables = names(df), run = 1L)
saveRDS(res_mp, "mix_mplusrev.RData")
desc <- mixtureSummaryTable(res_mp)
write.csv(desc, "supplemental_table_2.csv", row.names = FALSE)
classlabs <- paste0(c("High", "Low"), ": ", round(res_mp$mix_2_class.out$results$class_counts$modelEstimated$proportion*100), "%")
p <- plotGrowthMixtures(res_mp[[2]], rawdata = TRUE, bw = TRUE)
p <- p + scale_linetype_manual(labels = classlabs, values = c(1:2), name = "Risk", guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(labels = classlabs, values = c(15, 17), name = "Risk", guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = 0:4, labels = 14:18, expand = c(0, 0)) +
  labs(x = "Age", y = "Difficulties in emotion regulation") +
  theme(legend.position = c(0.88, 0.85))
saveRDS(p, "classplot.RData")
ggsave("classplot.pdf", p, device = "pdf", units = "in", width = 7, height = 5)

p <- plotGrowthMixtures(res_mp[[2]], rawdata = TRUE, alpha_range = c(0, 0.05))
p <- p + scale_linetype_manual(labels = classlabs, values = c(1:2), name = "Risk", guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(labels = classlabs, values = c(15, 17), name = "Risk", guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(labels = classlabs, values = c("green", "red"), name = "Risk", guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = 0:4, labels = 14:18, expand = c(0, 0)) +
  labs(x = "Age", y = "Difficulties in emotion regulation") +
  theme(legend.position = c(0.88, 0.85))
saveRDS(p, "classplot_color.RData")
