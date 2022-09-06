# random_4 <- sample(1:4, nrow(shapes), replace = TRUE)
# random_6 <- sample(1:6, nrow(shapes), replace = TRUE)

entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  
  sum(w * rowSums(e, na.rm = TRUE))
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  w <- table(cluster)/length(cluster)
  
  cnts <- sapply(split(truth, cluster), table)
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(w * apply(p, 1, max))
}

r <- rbind(
  kmeans_7 = c(
    unlist(fpc::cluster.stats(d1, km_income$cluster, truth, compareonly = TRUE)),
    entropy = entropy(km$cluster, truth),
    purity = purity(km$cluster, truth)
  ),
  hc_4 = c(
    unlist(fpc::cluster.stats(d1, clusters1_ward, truth, compareonly = TRUE)),
    entropy = entropy(hc_4, truth),
    purity = purity(hc_4, truth)
  )
  # random_4 = c(
  #   unlist(fpc::cluster.stats(d1, random_4, truth, compareonly = TRUE)),
  #   entropy = entropy(random_4, truth),
  #   purity = purity(random_4, truth)
  # ),
  # random_6 = c(
  #   unlist(fpc::cluster.stats(d1, random_6, truth, compareonly = TRUE)),
  #   entropy = entropy(random_6, truth),
  #   purity = purity(random_6, truth)
  )
)
r