### Emotional Dysregulation # Imputing data using missForest algorithm ###

# (missRanger is faster implementation of missForest)

########################################################################################################
# Over the following lines, code can be found in order to implement the missForest approach to data    #
# imputation. A random forest will be grown to build an imputation model.                              #
########################################################################################################

library(missRanger)
library(worcs)
try_numeric <- function(x){
  miss_x <- is.na(x)
  y <- tryCatch({as.numeric(x)},
                error = function(e){
                  x
                })
  miss_y <- is.na(y)
  if(!all(miss_y == miss_x)){
    return(y)
  }
  x
}

drop_floats <- function(x){
  tmp <- table(x)
  is_int <- as.numeric(names(tmp)) %% 1 == 0
  if(any(!is_int)){
    not_int <- which(x %% 1 != 0)
    x[not_int] <- NA
  }
  x
}
# Loading the data (important: First export the data in SPSS to a .csv file. The initial .sav data is 
#   password-protected. Save the file as <radardata.csv> in the subdirectory of the R-project)
df <- load_data(to_envir = FALSE)$df


# Fix problem with names --------------------------------------------------

names(df)[names(df) == "rir11aa19r"] <- "rir11aa19"

# Drop multiwave data, except for DERS
df[grepl("^.{2}[2-9]", names(df))& !grepl("^de", names(df))] <- NULL

# it would appear that some variables are recognized as characters/strings when read by R
#   however, all relevant variables should be numerical (mostly categorical, but numerical anyways)
indx1 <- sapply(df, is.character)
if(any(indx1)) browser()
#df[indx1] <- as.data.frame(lapply(df[indx1], function(x) as.numeric(x))) # Therefore those are changed to numeric

# Some errors appear to sit in the df (non-integer values for scales where only integers are allowed)
#   This is most likely due to imputation measures undertaken by the RADAR group (only for participants
#   with less than 10% of df missing, therefore both imputead AND missing df is included)

is_num <- sapply(df, inherits, what = c("numeric", "integer"))
df[!is_num] <- NULL

# tmp <- head(df[,!is_num])
# sapply(df[!is_num], class)
# tmp <- head(df[!is_num])
# df[indx2] <- as.data.frame(lapply(df[indx2], function(x) round(x))) # Rounding those values

# system and logical missings are recorded as 999 or 888 respectively.
for(i in 1:ncol(df)){
  df[which(df[,i] %in% c(888, 999)),i] <- NA #recoding 888 and 999 to NA
}
#test <- apply(df, 2, function(x) which(x==888|999))



# Screen missings ---------------------------------------------------------

miss <- is.na(df)
miss_row <- rowSums(miss)/ncol(miss)
miss_col <- colSums(miss)/nrow(miss)
hist(miss_row, 100)
hist(miss_col, 100)

df <- df[!miss_row > .6, ]
df <- df[, -which(miss_col > .6)]


# Prepare variables that are not straightforward scales -------------------

library(psych)
tmp <- df[, c("geslacht", grep("^pu11aa(01|04|05|09|10)$",names(df), value = T))]
V <- tmp[tmp$geslacht == 2, -1]
M <- tmp[tmp$geslacht == 1, -1]

miss <- is.na(M)
miss_row <- rowSums(miss)/ncol(miss)
miss_col <- colSums(miss)/nrow(miss)
hist(miss_row, 100)
hist(miss_col, 100)
range(miss_col)
M[, miss_col > .6] <-NULL
descriptives(M)
resM <- principal(cor(M, use = "pairwise.complete.obs"), 1)
rownames(resM$loadings)[resM$loadings[,1] > .5]
resV <- principal(V, 1)
rownames(resV$loadings)[resV$loadings[,1] > .5]

resM$loadings[,1][resM$loadings[,1] > .5]
resV$loadings[,1][resV$loadings[,1] > .5]

# Background variables ----------------------------------------------------
df$d_sib_age <- df$leeftijd_target_11 - df$leeftijd_sibling_11
df$d_par_age <- df$leeftijd_vader_11 - df$leeftijd_moeder_11
df$reli <- as.integer(!df$bg11aa04 == 8)
df$father <- df$bg11aa07 == 1
bg <- c("geslacht",
        "leeftijd_target_11",
        "leeftijd_moeder_11",
        "leeftijd_sibling_11",
        "leeftijd_vader_11",
        "d_sib_age",
        "d_par_age",
        "brpmoe_lmh",
        "brpvad_lmh",
        "sesgez_low",
        "bg11aa04", #religion Rooms katholiek Nederlands hervormd        Gereformeerd         Islamitisch       Hindoeistisch 
        #66                  49                  55                   4                   1 
        #Boedhistisch    Anders, namelijk  Ik heb geen geloof                 999 
        #0                  35                 281                   4
        "reli")


# Identifying relevant scales (names)
scalesrx <- read.csv("selected_scales.csv", stringsAsFactors = FALSE)
scalesrx <- scalesrx[-which(scalesrx$Drop), ]
scales_list <- lapply(scalesrx$item.names, function(x){strsplit(x, " ", fixed = TRUE)[[1]]})
names(scales_list) <- scalesrx$scale.name
scales_list[sapply(scales_list, length) == 0] <- NULL

# Extracting relevant scales using names identified in previous step
missingitems <- c(bg, unlist(scales_list))[!c(bg, unlist(scales_list)) %in% names(df)]
scales_list <- lapply(scales_list, function(x){
  x[!x %in% missingitems]
})


saveRDS(scales_list, "scales_list.RData")

# Select data
reldata <- df[, c(bg, unlist(scales_list))]

intvars <- names(reldata)[!sapply(reldata, is.integer)]
numvars <- c("leeftijd_target_11", "leeftijd_moeder_11", "leeftijd_sibling_11", 
             "leeftijd_vader_11", "d_sib_age", "d_par_age")
intvars <- intvars[!intvars %in% numvars]
reldata[intvars] <- lapply(reldata[intvars], drop_floats)
table(drop_floats(reldata$de21aa03))
#!
# @Caspar - make sure to adjust the num.trees parameter! 
#!

###################### Tuning parameters of missForest imputation algorithm ##################################

pmm.k <- 10 # number of non-missing values to be sampled for predictive mean mathching step (0 to avoid this step)
num.trees <- 50 # number of trees to be grown
maxiter <- 10 # maximum of iterations for forests to be grown (irrelevant if no meaningful changes 
              #   between iterations anymore) (e.g with 50 trees, algortithms stops after 3  iterations, as no
              #   improvement between iteration 2 and 3 took place)

##############################################################################################################



# apply missForest algorithm (using faster ranger implementation)
set.seed(205620)
reldataimp <- missRanger(reldata, pmm.k=pmm.k, num.trees=num.trees, maxiter=maxiter)
  
# Save imputed df (in .RData format! If imputed df is to be used elsewhere, save as csv or similar!)
saveRDS(reldataimp, "ImputedData.RData")


# briefly comparing missing vs imputed data set
table(reldata$an11aa01)
table(reldataimp$an11aa01)

table(reldata$de21aa03)
table(reldataimp$de21aa03)

