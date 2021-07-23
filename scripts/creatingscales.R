### Emotional Dysregulation # Creating Scales from Radar ###

#############################
# Lukas Beinhauer
# 08/07/20
#############################

########################################################################################################
# Over the following lines, code can be found in order to create scales from the synthetic RADAR data. #
# Important: run <missRanger.R> first!                                                                 #
# Scale scores are averages of each individual's item scores per scale/subscale                        #
########################################################################################################

library(tidySEM)
library(lavaan)
library(psych)
library(ggplot2)
library(semTools)
library(worcs)

data <- readRDS("ImputedData.RData") #load data

scales_list <- readRDS("scales_list.RData")


# Dependent variable ------------------------------------------------------

m_config <- character()
for(i in 2:6){
  m_config <- paste0(m_config,
                     paste0(
                       "de", i, " =~ ", 
                       paste0(grep(paste0("^de", i), names(data), value = TRUE), collapse = " + ")
                       ),
                     sep = "\n"
                     )
}
itemcors <- paste0(unlist(lapply(gsub(".{6}", "", grep("^de", names(data), value = TRUE)), function(i){
  #i = "03"
  m <- matrix(apply(expand.grid(paste0("de", 2:6, "1aa", i), "~~", paste0("de", 2:6, "1aa", i)), 1, paste0, collapse = " "), ncol = 5, byrow = TRUE)
  as.vector(m[lower.tri(m)])
})), collapse = "\n")
m_config <- paste0(m_config, itemcors)

r_config <- sem(m_config, data)

df_plot <- parameterestimates(r_config)
df_plot <- df_plot[df_plot$lhs %in% paste0("de", 2:6) & !df_plot$rhs %in% paste0("de", 2:6), ]
df_plot$rhs <- ordered(as.integer(gsub("^.{6}", "", df_plot$rhs)))

ggplot(df_plot, aes(y = rhs, x = est)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper)) +
  facet_wrap(~lhs)

ggplot(df_plot, aes(y = rhs, x = est, shape = lhs)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height = 0, size = 4, alpha = .2)

m_metric <- character()
for(i in 2:6){
  m_metric <- paste0(m_metric,
                     paste0(
                       "de", i, " =~ ", 
                       paste0(
                         gsub("^.{6}", "i", grep(paste0("^de", i), names(data), value = TRUE)),
                         " * ",
                         grep(paste0("^de", i), names(data), value = TRUE)
                         , collapse = " + ")
                     ),
                     sep = "\n"
  )
}

m_metric <- paste0(m_metric, itemcors)

r_metric <- sem(m_metric, data)


anova(r_config, r_metric)
compareFit(r_config, r_metric)
scores <- lavPredict(r_metric)
head(scores)
plot(1:5, scores[8,])
fitmeasures(r_config)[c("rmsea", "cfi", "tli")]


# Prepare predictor scales ------------------------------------------------
unlist(scales_list)[!unlist(scales_list) %in% names(data)]
pred_list <- lapply(scales_list, function(x){ prcomp(data[x]) })

pc_mat <- t(sapply(pred_list, function(x){
  tst <- summary(x)
  out <- rep(NA, 6)
  out[1:min(c(6, ncol(tst$importance)))] <- tst$importance[2, 1:min(c(6, ncol(tst$importance)))]
  out
}))
#pc_mat <- pc_mat[!is.na(pc_mat[, 2]), ]
range(pc_mat[,1][!pc_mat[,1]==1])

#library(psych)
#fa.parallel(data[scales_list$schoolprestatie_aa], fa = "pc", plot = FALSE)
#summary(prcomp(data[scales_list$schoolprestatie_aa]))

scores_pred <- data.frame(sapply(pred_list, function(x){x$x[, 1]}))


# Create data -------------------------------------------------------------

df_anal <- data.frame(scores,
                      data[, which(!names(data) %in% unlist(scales_list))],
                      scores_pred)

res <- lavaan::growth("# intercept and slope with fixed coefficients 
                       i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
                       s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6",
               df_anal)

fitMeasures(res)
standardizedSolution(res)
lavInspect(res, "r2")

closed_data(df_anal)