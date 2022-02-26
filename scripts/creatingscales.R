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

ders_desc <- lapply(2:6, function(i){
  df_tmp <- data[grep(paste0("^de", i), names(data), value = TRUE)]
  mod <- paste0("F =~ ", paste0(names(df_tmp), collapse = "+"))
  res_cfa <- cfa(mod, data)
  res_paral <- fa.parallel(df_tmp)
  res_alpha <- psych::alpha(df_tmp)
  list(cfa = res_cfa, paral = res_paral, alpha = res_alpha)
})
saveRDS(ders_desc, "ders_desc.RData")
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
revcod <- sapply(scales_list, function(x){ 
  pc <- prcomp(data[x]) 
  signs <- sign(pc$rotation[,1])
  tb <- table(signs)
  tb <- sort(tb, decreasing = TRUE)
  names(tb)[1] == "-1"
  })
# Note: Neuroticism and self-concept clarity were already reverse-coded;
# this reverse coding has flipped them the right way around. Thus,
# remove them from this list.
saveRDS(names(revcod)[revcod], "revcod.RData")

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
reldata <- readRDS("reldata.RData")

scales_list_desc <- lapply(scales_list, function(i){
  if(any(!i %in% names(reldata))){
    NULL
  } else {
    i
  }
})
scales_list_desc <- scales_list_desc[!sapply(scales_list_desc, is.null)]
# Descriptive table -------------------------------------------------------
renm <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = F)
# Scales
desc_scales <- tidySEM:::create_scales.data.frame(reldata, keys.list = scales_list_desc)$descriptives
pc1 <- pc_mat[,1]
names(pc1) <- rownames(pc_mat)
desc_scales$PC1 <- formatC(pc1[desc_scales$Subscale], digits = 2, format = "f")
desc_scales[c("n", "Items")] <- lapply(desc_scales[c("n", "Items")], as.integer)

# Individual items
data_noscale2 <- reldata[, which(!names(reldata) %in% unlist(scales_list))]
data_noscale2$cigarettes <- as.numeric(reldata$su11aa04)
data_noscale2$geslacht <- factor(data_noscale2$geslacht, labels = c("Boy", "Girl"))
data_noscale2$brpmoe_lmh <- factor(data_noscale2$brpmoe_lmh, labels = c("low", "medium", "high"))
data_noscale2$brpvad_lmh <- factor(data_noscale2$brpvad_lmh, labels = c("low", "medium", "high"))
data_noscale2$sesgez_low <- factor(data_noscale2$sesgez_low, labels = c("medium/high", "low SES"))
data_noscale2$bg11aa04 <- factor(data_noscale2$bg11aa04, labels = c("Catholic", "Dutch reformed", "Reformed", "Islam", 
  "Hindu", "Other", "Atheist"))
data_noscale2$reli <- factor(data_noscale2$reli, labels = c("No", "Yes"))
desc_noscale <- descriptives(data_noscale2)
desc_noscale$mode[is.na(desc_noscale$mode_value)] <- NA
desc_noscale$mode <- as.integer(desc_noscale$mode)

desc_num <- desc_noscale[desc_noscale$type == "numeric", -c(2, 4:5, 7:9,11, 14)]
names(desc_scales)[1] <- "name"
desc_num <- tidySEM:::bind_list(list(desc_num, desc_scales))
desc_num[c("skew", "kurt")] <- NULL
desc_num[c("mean", "sd", "min", "max", "skew_2se", "kurt_2se", "min_load", "max_load", "PC1")] <- lapply(desc_num[c("mean", "sd", "min", "max", "skew_2se", "kurt_2se", "min_load", "max_load", "PC1")], as.numeric)
desc_num$name <- renm$V2[match(desc_num$name, renm$V1)]
write.csv(desc_num, "desc_num.csv", row.names = FALSE)

desc_cat <- desc_noscale[!desc_noscale$type == "numeric", -2]
desc_cat$unique <- sapply(desc_cat$name, function(i){length(table(reldata[[i]]))})
desc_cat <- desc_cat[inverse.rle(list(lengths = desc_cat$unique, values = 1:nrow(desc_cat))), ]
desc_cat <- desc_cat[, !sapply(desc_cat, function(i){all(is.na(i))})][, -3]
desc_cat <- cbind(desc_cat, do.call(rbind, lapply(unique(desc_cat$name), function(n){
  as.data.frame.table(prop.table(table(data_noscale2[[n]])))
})))
desc_cat <- desc_cat[, c("name", "n", "Freq", "Var1")]
names(desc_cat) <- c("name", "n", "%", "Category")
desc_cat$name <- renm$V2[match(desc_cat$name, renm$V1)]
write.csv(desc_cat, "desc_cat.csv", row.names = FALSE)

additionalinfo <- read.csv("selected_scales.csv", stringsAsFactors = F)
names(additionalinfo)[names(additionalinfo) == "scale.name"] <- "name"
tabsup1 <- merge(desc_scales, additionalinfo, by = "name", all.x = TRUE)
names(tabsup1)[names(tabsup1) == "name"] <- "variable.name"
tabsup1$Scale.name <- renm$V2[match(tabsup1$variable.name, renm$V1)]
tabsup1 <- tabsup1[, c("Scale.name", "variable.name", "Items", "n", "mean", "sd", "min", "max", 
                       "skew", "skew_2se", "kurt", "kurt_2se", "Reliability", "Interpret", 
                       "min_load", "max_load", "PC1", "Reference", "Measure", 
                       "Scale", "construct", "description", 
                       "sample.item", "items.regex", "subscale", "comments", "item.names")]
write.csv(tabsup1, "supplemental_table_1.csv", row.names = FALSE)

# Create data -------------------------------------------------------------

df_anal <- data.frame(scores,
                      data_noscale,
                      scores_pred)

res <- lavaan::growth("# intercept and slope with fixed coefficients 
                       i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
                       s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6
                       q =~ 0*de2 + 1*de3 + 4*de4 + 9*de5 + 16*de6",
               df_anal)

fitMeasures(res)
standardizedSolution(res)
lavInspect(res, "r2")

closed_data(df_anal)