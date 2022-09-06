#Report
library("ggpubr")
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("cluster")
library("xlsx")
library(scales)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra) 
library("factoextra") 
getwd()
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/project2")
income_education_scaled_clean <- read.csv("income_education_scaled_clean.csv")
hospital_age_commuter_householder_scaled_clean <- read.csv("hospital_age_commuter_householder_scaled_clean.csv")
summary(income_education_scaled_clean)
summary(hospital_age_commuter_householder_scaled_clean)
#df_income_education_TX_scaled <- df_income_education_TX[,-1] %>%na.omit() %>%scale() %>% as_tibble()
#summary(df_income_education_TX_scaled)
#df_hospital_age_commuter_householder_TX_scaled <- df_hospital_age_commuter_householder_TX[,-1] %>%na.omit() %>%scale() %>% as_tibble()
#summary(df_hospital_age_commuter_householder_TX_scaled)
library(fpc)
library(cluster)
library(dbscan)
#LOF
#lof <- lof(df_income_education_TX_scaled,minPts = 10)
#lof
#hist(lof, breaks = 10, main = "LOF (minPts = 10)")
#income_education_scaled_clean <- df_income_education_TX_scaled %>%
#  add_column(lof = lof) %>%
#  filter(lof < 1.4)#try1.8
#income_education_scaled_clean <- select(income_education_scaled_clean,-lof)

#lof <- lof(df_hospital_age_commuter_householder_TX_scaled,minPts = 10)
#lof
#hist(lof, breaks = 10, main = "LOF (minPts = 10)")
#hospital_age_commuter_householder_scaled_clean <- df_hospital_age_commuter_householder_TX_scaled %>%
#  add_column(lof = lof) %>%
#  filter(lof < 1.5)#try2.3
#hospital_age_commuter_householder_scaled_clean <- select(hospital_age_commuter_householder_scaled_clean,-lof)
#PAM
#pamk.best <- pamk(income_education_scaled_clean,krange=2:10)
#pamk.best
#pam_income_education <- pam(dist_in_ed_clean, k = pamk.best$nc)
#clusplot(pam_income_education)
#pam_income_education <- pam(income_education_scaled_clean, k = pamk.best$nc) %>%clusplot(,shade = T, color = T)
#pam_income_education$silinfo$avg.width#pam's Silhouette coefficient
#pam_income_education$silinfo


library(factoextra)
library(ggplot2)
#set.seed(1234)
fviz_nbclust(hospital_age_commuter_householder_scaled_clean, pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
ggsave("wss_hospital_age_commuter_householder_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

fviz_nbclust(hospital_age_commuter_householder_scaled_clean, pam, method = "silhouette")
ggsave("silhouette_hospital_age_commuter_householder_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

fviz_nbclust(hospital_age_commuter_householder_scaled_clean, pam, method = "gap_stat")
ggsave("gap_stat_hospital_age_commuter_householder_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
#Thus we use 2 clusters


dist_hospital_clean <- dist(hospital_age_commuter_householder_scaled_clean)
p <- pam(dist_hospital_clean,k = 2)
p
#plot(p)
fviz_silhouette(silhouette(p$cluster, dist_hospital_clean))
ggsave("Silhouette_plot_hospital_age_commuter_householder_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

hospital_age_commuter_householder_scaled_clean_clusterd<- hospital_age_commuter_householder_scaled_clean %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(hospital_age_commuter_householder_scaled_clean[p$medoids, ], rownames = "cluster")
medoids
## __Note:__ `fviz_cluster` needs the original data.
fviz_cluster(c(p, list(data = hospital_age_commuter_householder_scaled_clean)), geom = "point", ellipse.type = "norm")
#fviz_cluster(c(p, list(data = hospital_age_commuter_householder_scaled_clean)), geom = "point", ellipse.type = "norm")
ggsave("Cluster_plot_hospital_age_commuter_householder_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
fpc::cluster.stats(dist_hospital_clean, p$cluster)

#External vaild
p$cluster#method's cluster
#Raw_truth <- read.csv("subset2_with_death_rate.csv")
#read the subset with death_per_cases
#summary(Raw_truth)
Raw_truth <- read.xlsx("subset2_with_death_rate.xlsx",sheetIndex = 1)#choose one
#summary(Raw_truth)
Raw_truth$pam_cluster <- p$cluster#add col with your methods name
#summary(Raw_truth)
write.xlsx(Raw_truth,"subset2_PAM_with_death_rate.xlsx")#write to xlsx and add a truth col

pam_subset2_valid_data <- read.xlsx("subset2_PAM_add_cluster_with_death_rate.xlsx",sheetIndex = 1)#read it
truth <- pam_subset2_valid_data %>% select("truth")#select truth
truth

t<-unlist(truth) #unlit
t
truth <- as.integer(t)
truth

#truth <- as.integer(truth)
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
  pam_subset2_k_2 = c(
    unlist(fpc::cluster.stats(d, p$cluster, truth, compareonly = TRUE)),
    entropy = entropy(p$cluster, truth),
    purity = purity(p$cluster, truth)
  ))
r
truth <- rev(truth)
r

fviz_nbclust(income_education_scaled_clean, pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
ggsave("wss_income_education_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

fviz_nbclust(income_education_scaled_clean, pam, method = "silhouette")
ggsave("silhouette_income_education_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

fviz_nbclust(income_education_scaled_clean, pam, method = "gap_stat")
ggsave("gap_stat_income_education_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
#Thus we use 2 clusters
dist_in_ed_clean <- dist(income_education_scaled_clean)
p <- pam(dist_in_ed_clean,k = 2)
p
#plot(p)
fviz_silhouette(silhouette(p$cluster, dist_in_ed_clean))
ggsave("Silhouette_plot_income_education_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")

income_education_scaled_clean_clusterd<- income_education_scaled_clean %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(income_education_scaled_clean[p$medoids, ], rownames = "cluster")
medoids
## __Note:__ `fviz_cluster` needs the original data.
fviz_cluster(c(p, list(data = income_education_scaled_clean)), geom = "point", ellipse.type = "norm")
#fviz_cluster(c(p, list(data = hospital_age_commuter_householder_scaled_clean)), geom = "point", ellipse.type = "norm")
ggsave("Cluster_plot_income_education_scaled_clean.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
fpc::cluster.stats(dist_in_ed_clean, p$cluster)

library(mclust)
### try Mclust
m_clust <- Mclust(as.matrix(income_education_scaled_clean),G=1:20)
summary(m_clust)
fviz_cluster(m_clust,income_education_scaled_clean)
ggsave("subset1_MCluster.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
plot(m_clust, "BIC")
ggsave("subset1_MCluster_BIC.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
m_clust <- Mclust(as.matrix(hospital_age_commuter_householder_scaled_clean),G=1:20)
fviz_cluster(m_clust,hospital_age_commuter_householder_scaled_clean)
ggsave("subset2_MCluster.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")
m_clust
summary(m_clust)
plot(m_clust, "BIC")
ggsave("subset2_MCluster_BIC.jpeg",width = 2560, height = 1600, units = "px", dpi = "retina")



