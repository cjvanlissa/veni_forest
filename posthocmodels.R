library(tidySEM)
vim <- readRDS("results/vim_all_22021-08-21_17_23_27.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
preds <- names(sort(VI$variable.importance, decreasing = TRUE))

tab <- lapply(preds, function(p){
  model <- "i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6
q =~ 0*de2 + 1*de3 + 4*de4 + 9*de5 + 16*de6\n"
  if(inherits(df_anal[[p]], "factor")){
    if(length(table(df_anal[[p]])) == 2){
      levels(df_anal[[p]]) <- c("0", "1")
      df_anal[[p]] <- as.numeric(as.character(df_anal[[p]]))
    }
  }
    
  if(!inherits(df_anal[[p]], "factor")){
    model <- paste0(model,
      paste0("i ~ ", p, "\n", "s ~ ", p, "\n", "q ~ ", p))
    res_preds <- lavaan::growth(model, data = df_anal)
    tab <- table_results(res_preds, columns = NULL)
    tab <- tab[(tab$lhs %in% c("i", "s", "q") & tab$rhs %in% preds), ]
    tab_coef <- unlist(tab[["est_sig_std"]])
    names(tab_coef) <- unlist(tab$lhs)
    tab_coef <- tab_coef[c("i", "s", "q")]
  } else {
    grpsize <- table(df_anal[[p]])
    grpnams <- names(grpsize)[grpsize > 50]
    tmp <- df_anal
    tmp <- tmp[tmp[[p]] %in% grpnams, ]
    model <- model <- paste0(model,
                             paste0("i ~ c(",
                                    paste0("ci", grpnams, collapse = ","),
                                    ") *1\n",
                                    "s ~ c(",
                                    paste0("cs", grpnams, collapse = ","),
                                    ") *1\n",
                                    "q ~ c(",
                                    paste0("cq", grpnams, collapse = ","),
                                    ") *1\n"))
    
    res_preds <- lavaan::growth(model, data = tmp, group = p)
    res_coni <- lavaan::growth(gsub("ci.+?\\b", "ci", model), data = tmp, group = p)
    pi <- semTools::compareFit(res_preds, res_coni)@nested[2,7]
    res_cons <- lavaan::growth(gsub("cs.+?\\b", "cs", model), data = tmp, group = p)
    ps <- semTools::compareFit(res_preds, res_cons)@nested[2,7]
    res_conq <- lavaan::growth(gsub("cq.+?\\b", "cq", model), data = tmp, group = p)
    pq <- semTools::compareFit(res_preds, res_conq)@nested[2,7]
    tab_coef <- tidySEM:::est_sig(rep("", 3), sig = c(pi, ps, pq))
    names(tab_coef) <- c("i", "s", "q")
  }
  fits <- table_fit(res_preds)
  c(list(Predictor = p), as.list(tab_coef), fits[c("rmsea", "cfi", "srmr")])
})
tab <- do.call(rbind, lapply(tab, as.data.frame))
any(tab$rmsea > .08 | tab$cfi < .95 | tab$srmr > .08)
ren <- read.csv("scale_rename.csv", stringsAsFactors = F, header = FALSE)
tab$Predictor <- ren$V2[match(tab$Predictor, ren$V1)]
tmp <- matrix(grepl("*", unlist(tab[, 2:4]), fixed = T), ncol = 3)
colSums(tmp)/nrow(tmp)
write.csv(tab[, 1:4], "supplemental_table_3.csv", row.names = FALSE)