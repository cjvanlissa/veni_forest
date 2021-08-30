formals(semtree::diversityMatrix)()
library(future)
library(ggplot2)
res_rf <- readRDS("results/full_forest_all_2.RData")

plan(multisession, workers = 2, gc = TRUE)
plan(sequential)
simmat <- similarityMatrix(res_rf)
library(worcs)
df_anal <- worcs::load_data(to_envir = FALSE)$df_anal
simmat <- readRDS("sim_mat.RData")
library(fpc)
library(dbscan)
fpc::dbscan()

D <- 1-simmat
d=as.dist(D)
hc=hclust(d,method="complete")
plot(hc)
numk <- 5
clusts <- cutree(hc, k = numk)

df <- data.frame(res_rf$data[grep("^de\\d$", names(res_rf$data))], cluster = clusts)
library(lavaan)
mod <- "i =~ 1*de2 + 1*de3 + 1*de4 + 1*de5 + 1*de6
s =~ 0*de2 + 1*de3 + 2*de4 + 3*de5 + 4*de6
q =~ 0*de2 + 1*de3 + 4*de4 + 9*de5 + 16*de6
i ~ meani*1
s ~ means*1
q ~ meanq*1
de2 ~~ vres*de2
de3 ~~ vres*de3
de4 ~~ vres*de4
de5 ~~ vres*de5
de6 ~~ vres*de6
de2 ~ 0*1
de3 ~ 0*1
de4 ~ 0*1
de5 ~ 0*1
de6 ~ 0*1
i ~~ cis*s
i ~~ 0*q
s ~~ 0*q
i ~~ vi*i
s ~~ vs*s
q ~~ 0*q"
mod <- gsub("\\b([a-z]+)\\b\\*", paste0("c\\(", paste0("\\1", 1:numk, collapse = ", "), "\\)\\*"), mod)
m1 <- growth(mod, data = df, group = "cluster")

pt <- parameterestimates(m1)
pt <- pt[startsWith(pt$label, "mean"), ]
pt <- pt[, c("label", "est")]
pt$Group <- gsub("^.+?(.)$", "\\1", pt$label)
pt$label <- gsub("^(.+?).$", "\\1", pt$label)
times <- matrix(c(rep(1, 5), 0:4, (0:4)^2), nrow = 5)
df_plot <- do.call(rbind, lapply(unique(pt$Group), function(i){
  data.frame(Group = i, y = rowSums(matrix(pt$est[pt$Group == i], nrow(times), ncol(times), byrow = TRUE) * times), Time = 1:nrow(times))
}))
prop.table(table(clusts))
ggplot(df_plot, aes_string(x = "Time", y = "y", shape = "Group", group = "Group", linetype = "Group")) +
  geom_path() +
  geom_point() +
  theme_bw()

fpc::dbscan(data = 1-simmat, method = "dist")
set.seed(123)
db <- fpc::dbscan(data = D, method = "dist", eps = 0.5, MinPts = 5)
# Plot DBSCAN results
plot(db, D, main = "DBSCAN", frame = FALSE)
dbscan::kNNdistplot(D, k =  5)
abline(h = 0.15, lty = 2)

similarityMatrix <- function(forest, ...){
  combs <- combn(1:nrow(forest$data), 2)
  dat <- forest$data
  forst <- forest$forest
  vals <- future.apply::future_lapply(FUN = function(tree){
    vec <- semtree:::traverse(tree = tree, data = dat)
    vec[combs[1,]] == vec[combs[2,]]
    }, X = forst)
  vals <- do.call(cbind, vals)
  vals <- rowMeans(vals)
  locs <- cbind(combs, combs[c(2,1), ], matrix(1:nrow(dat), ncol = nrow(dat), nrow = 2, byrow = TRUE))
  vals <- c(vals, vals, rep(1, nrow(dat)))
  as.matrix(Matrix::sparseMatrix(i = locs[1,], j = locs[2, ], x = vals))
}
