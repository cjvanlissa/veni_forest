# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
library(foreign)
imp_fun <- function(x){
  UseMethod("imp_fun", x)
}
imp_fun.data.frame <- function(x){
  data.frame(lapply(x, imp_fun))
}
imp_fun.default <- function(x){
  out <- x
  if(inherits(x, "numeric")){
    out[is.na(out)] <- mean(x[!is.na(out)])
  } else {
    out[is.na(out)] <- names(sort(table(out), decreasing = TRUE))[1]
    if(!class(out) == class(x)){
      tryCatch({
        out <- do.call(paste0("as.", class(x)[1]), list(out))
      }, error = function(e){
        stop("Could not impute missing values.")
      })
    }
  }
  out
}

df <- read.spss("DADradar-27-07-2020-Van_Lissa_2.sav", to.data.frame = TRUE, use.value.labels = FALSE)
names(df) <- tolower(names(df))
# Pseudonymize
df[c("rpp", "bg41aa14b3", grep("^id", names(df), value = TRUE))] <- NULL

# Check descriptives
desc <- descriptives(df)
desc[which(desc$unique == 1), ]
# table(df$rpp)

# Store data
closed_data(df, codebook = NULL, synthetic = FALSE)