library(tidyverse)
library(ggplot2)
library(factoextra)
library(fpc)
library(cluster)

library(dplyr)
library(graphics)

census <- read.csv("counties_information.csv")


census <- as_tibble(census)
census <- census %>% mutate_if(is.character,factor)
census_TX <- census %>% filter(state == "TX")
summary(census_TX)

### all_feature table
all_features <- census_TX %>% 
  filter(confirmed_cases > 100) %>% 
  select(county_name, confirmed_cases, deaths, total_pop,
         median_income,income_per_capita,poverty,
         employed_pop,bachelors_degree_or_higher_25_64,
         male_65_to_66,male_67_to_69,male_70_to_74,male_75_to_79,male_80_to_84,male_85_and_over,
         female_65_to_66,female_67_to_69,female_70_to_74,female_75_to_79,female_80_to_84,female_85_and_over,
         households,commuters_by_public_transportation,num_hospital
  ) %>%
  as_tibble()
write.csv(all_features,file = "all_features.csv",row.names = FALSE)

### incomd_education table
income_education <- all_features %>%
  mutate(
    cases_per_1000 = confirmed_cases/total_pop*1000,
    deaths_per_1000 = deaths/total_pop*1000,
    deaths_per_case = deaths/confirmed_cases,
    
    poverty_percentage = poverty/total_pop,
    employed_pop_percentage = employed_pop/total_pop,
    bachelors_degree_or_higher_25_64_percentage = bachelors_degree_or_higher_25_64/total_pop,
  )%>%
  select(
    county_name,cases_per_1000,deaths_per_1000,deaths_per_case,
    poverty_percentage,employed_pop_percentage,bachelors_degree_or_higher_25_64_percentage,
    median_income,income_per_capita
         )%>%
  as_tibble()
write.csv(income_education,file = "income_education.csv",row.names = FALSE)

### hospital_age_commuter_householder table
hospital_age_commuter_householder <- all_features %>%
  mutate(
    cases_per_1000 = confirmed_cases/total_pop*1000,
    deaths_per_1000 = deaths/total_pop*1000,
    deaths_per_case = deaths/confirmed_cases,
    
    commuters_by_public_transportation_per_1000 = commuters_by_public_transportation/total_pop*1000,
    households_percentage = households/total_pop,
    male_65_to_more = male_65_to_66 + male_67_to_69,male_70_to_74 + male_75_to_79 + male_80_to_84 + male_85_and_over,
    female_65_to_more = female_65_to_66 + female_67_to_69 + female_70_to_74 + female_75_to_79 + female_80_to_84 + female_85_and_over,
    pop_65_to_more = male_65_to_more + female_65_to_more,
    pop_65_to_more_percentage = pop_65_to_more/total_pop,
    num_hospital_per_1000 = num_hospital/total_pop*1000
  )%>%
  select(
    county_name,cases_per_1000,deaths_per_1000,deaths_per_case,
    commuters_by_public_transportation_per_1000,households_percentage,pop_65_to_more_percentage,
    num_hospital_per_1000
  )%>%
  as_tibble()
write.csv(hospital_age_commuter_householder,file = "hospital_age_commuter_householder.csv",row.names = FALSE)

#### Test
Test1 <- all_features %>%
  mutate(
    cases_per_1000 = confirmed_cases/total_pop*1000,
    deaths_per_1000 = deaths/total_pop*1000,
    deaths_per_case = deaths/confirmed_cases,
    
    poverty_percentage = poverty/total_pop,
    employed_pop_percentage = employed_pop/total_pop,
    num_hospital_per_1000 = num_hospital/total_pop*1000,
    households_percentage = households/total_pop
  )%>%
  select(
    county_name,cases_per_1000,deaths_per_1000,deaths_per_case,
    poverty_percentage,employed_pop_percentage,num_hospital_per_1000,
    households_percentage
  )%>%
  as_tibble()
Test1_scaled <- Test1 %>%
  select(
    poverty_percentage,
    employed_pop_percentage,
    num_hospital_per_1000,
    households_percentage,
  )%>%
  scale()%>%
  as_tibble()
summary(Test1_scaled)

# Hopkins
get_clust_tendency(Test1_scaled,n=10)
get_clust_tendency(income_education_scaled_clean,n=10)
get_clust_tendency(hospital_age_commuter_householder_scaled_clean,n=10)

### scale data
income_education_scaled <- income_education %>%
  select(
    poverty_percentage,
    employed_pop_percentage,
    bachelors_degree_or_higher_25_64_percentage,
    median_income,
    income_per_capita
  )%>%
  scale()%>%
  as_tibble()

hospital_age_commuter_householder_scaled <- hospital_age_commuter_householder %>%
  select(
    commuters_by_public_transportation_per_1000,
    households_percentage,
    pop_65_to_more_percentage,
    num_hospital_per_1000
  )%>%
  scale()%>%
  as_tibble()%>%
  # delete  rows containing NA
  na.omit()


### try Mclust
m_clust <- Mclust(as.matrix(income_education_scaled_clean),G=1:20)
fviz_cluster(m_clust,income_education_scaled_clean)
m_clust <- Mclust(as.matrix(hospital_age_commuter_householder_scaled_clean),G=1:20)
fviz_cluster(m_clust,hospital_age_commuter_householder_scaled_clean)

summary(m_clust)
plot(m_clust, "BIC")

### determine a suitable number of clusters

# Elbow method (look at the knee)
fviz_nbclust(income_education_scaled_clean,hcut,method = "wss") +
  geom_vline(xintercept = 3,linetype = 2)
fviz_nbclust(hospital_age_commuter_householder_scaled_clean,hcut,method = "wss") +
  geom_vline(xintercept = 2,linetype = 2)

# compare different methods
ks <- 2:10
# Average silhouette
fviz_nbclust(income_education_scaled_clean, hcut, method = "silhouette")
fviz_nbclust(hospital_age_commuter_householder_scaled_clean, hcut, method = "silhouette")

fviz_nbclust(income_education_scaled_clean, kmeans, method = "silhouette")
fviz_nbclust(hospital_age_commuter_householder_scaled_clean, kmeans, method = "silhouette")

d <- d1
m <- "ward.D2"
# Average Silhouette Width
ASW <- sapply(ks, function(k){
  hc<- hclust(d,method = m)
  clusters = cutree(hc,k)
  cluster.stats(d1,clusters)$avg.silwidth
})

ASW
best_k <- ks[which.max(ASW)]
best_k
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)

k <- 2:20
wcss <- sapply(k, FUN = function(k) {kmeans(income_education_scaled_clean, centers = k, nstart = 10)$avg.silwidth})
ggplot(as_tibble(k, wcss), aes(k, wcss)) + geom_line() 

ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(income_education_scaled_clean, centers=k, nstart = 10)$cluster)$avg.silwidth
})

best_k <- ks[which.max(ASW)]
best_k
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)

# Gap statistic
gap_stat <- clusGap(income_education_scaled_clean, FUN = hcut, K.max = 10, B = 50, method = "Tibs2001SEmax",SE.factor = 2)
gap_stat <- clusGap(income_education_scaled_clean, FUN = kmeans, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
plot(gap_stat)

gap_stat <- clusGap(hospital_age_commuter_householder_scaled_clean, FUN = hcut, K.max = 10, B = 50)
gap_stat <- clusGap(hospital_age_commuter_householder_scaled_clean, FUN = kmeans, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
plot(gap_stat)

# Dunn Index
ks <- 2:10

DI <- sapply(ks, FUN=function(k) {
  clusters = cutree(hc1_complete,k)
  cluster.stats(d1,clusters)$dunn
})
DI <- sapply(ks, FUN=function(k) {
  clusters = cutree(hc1_ward,k)
  cluster.stats(d1,clusters)$dunn
})
DI <- sapply(ks, FUN=function(k) {
  clusters = cutree(hc2_complete,k)
  cluster.stats(d2,clusters)$dunn
})
DI <- sapply(ks, FUN=function(k) {
  clusters = cutree(hc2_ward,k)
  cluster.stats(d2,clusters)$dunn
})
best_k <- ks[which.max(DI)]
best_k
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)

### Check outlier
lof <- lof(income_education_scaled,minPts = 10)

lof
hist(lof, breaks = 10, main = "LOF (minPts = 10)")

income_education_scaled_clean <- income_education_scaled %>%
  add_column(lof = lof) %>%
  filter(lof < 1.4)%>%
  select(-lof)
write.csv(income_education_scaled_clean,file = "income_education_scaled_clean.csv",row.names = FALSE)

lof <- lof(hospital_age_commuter_householder_scaled,minPts = 10)
hospital_age_commuter_householder_scaled_clean <- hospital_age_commuter_householder_scaled %>%
  add_column(lof = lof) %>%
  filter(lof < 1.5)%>%
  select(-lof)
write.csv(hospital_age_commuter_householder_scaled_clean,file = "hospital_age_commuter_householder_scaled_clean.csv",row.names = FALSE)

ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.5, color = "red", linetype = 2)
# 
# x <- income_education_scaled_clean$median_income
# y <-income_education_scaled_clean$poverty_percentage
# ggplot(income_education_scaled_clean %>% add_column(outlier = lof >= 1.8), aes(x, y, color = outlier)) +
#   geom_point()
# 
# x <- income_education_scaled_clean$employed_pop_percentage
# y <-income_education_scaled_clean$bachelors_degree_or_higher_25_64_percentage
# ggplot(income_education_scaled_clean %>% add_column(outlier = lof >= 1.8), aes(x, y, color = outlier)) +
#   geom_point()

outlier.scores <- lofactor(income_education_scaled,k=5)
plot(density(outlier.scores))
outliers <- order(outlier.scores,decreasing = T)[1:20]
print(outliers)

outlier.scores <- lofactor(hospital_age_commuter_householder_scaled,k=5)
plot(density(outlier.scores))
outliers <- order(outlier.scores,decreasing = T)[1:22]
print(outliers)
# print(income_education_scaled[outliers,])
# n <- nrow(income_education_scaled)
# labels <- 1:n
# labels[-outliers] <- "."
# biplot(prcomp(income_education_scaled),cex = .8,xlabs = labels)

pch <- rep(".",n)
pch[outliers] <- "+"
col <- rep("black",n)
col[outliers] <- "red"

pairs(income_education_scaled,pch=pch,col=col)
pairs(hospital_age_commuter_householder_scaled,pch=pch,col=col)

### hierarchical clustering
?dist
d1 <- dist(income_education_scaled_clean)
d2 <- dist(hospital_age_commuter_householder_scaled_clean)

hc1_complete <- hclust(d1,method = "complete")
hc1_single <- hclust(d1,method = "single")
hc1_ward <- hclust(d1,method = "ward.D2")

hc2_complete <- hclust(d2,method = "complete")
hc2_single <- hclust(d2,method = "single")
hc2_ward <- hclust(d2,method = "ward.D2")

plot(hc1_complete,hang = -1,labels = FALSE)
plot(hc1_single,hang = -1,labels = FALSE)
plot(hc1_ward,hang = -1,labels = FALSE)

plot(hc2_complete,hang = -1,labels = FALSE)
plot(hc2_single,hang = -1,labels = FALSE)
plot(hc2_ward,hang = -1,labels = FALSE)
?plot
install.packages("sparcl")
install.packages('ggdendro')
library(ggdendro)
library(sparcl)

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram

fviz_dend(hc1_complete, k = 3,show_labels = FALSE)
fviz_dend(hc1_single, k = 3,show_labels = FALSE)
fviz_dend(hc1_ward, k = 3,show_labels = FALSE)

fviz_dend(hc2_complete, k = 4,show_labels = FALSE)
fviz_dend(hc2_single, k = 4,show_labels = FALSE)
fviz_dend(hc2_ward, k = 4,show_labels = FALSE)

### internal validation measure -> compare
k1 <- 3
clusters1_complete = cutree(hc1_complete,k1)
clusters1_ward = cutree(hc1_ward,k1)
k2 <- 4
clusters2_complete = cutree(hc2_complete,k2)
clusters2_ward = cutree(hc2_ward,k2)

fpc::cluster.stats(d1, clusters1_ward)
# WCSS & average silhouette width
sapply(
  list(
    clusters1_complete,
    clusters1_ward
  ),
  FUN = function(x)
    fpc::cluster.stats(d1, x))[c("within.cluster.ss", "avg.silwidth"), ]

sapply(
  list(
    clusters2_complete,
    clusters2_ward
  ),
  FUN = function(x)
    fpc::cluster.stats(d2, x))[c("within.cluster.ss", "avg.silwidth"), ]

# Silhouette plot
fviz_silhouette(silhouette(clusters1_complete, d1))
fviz_silhouette(silhouette(clusters1_ward, d1))

fviz_silhouette(silhouette(clusters2_complete, d2))
fviz_silhouette(silhouette(clusters2_ward, d2))

# Cluster Profiles
# Inspect the centroids with horizontal bar charts organized by cluster.
hc1_ward

cluster_center <- aggregate(income_education_scaled_clean,list(cluster=clusters1_ward),mean) %>%
  select(-cluster)

ggplot(pivot_longer(as_tibble(cluster_center,  rownames = "cluster"), cols = colnames(cluster_center), names_to = "feature"),
       aes(x = value, y = feature, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))


hc2_complete

cluster_center <- aggregate(hospital_age_commuter_householder_scaled_clean,list(cluster=clusters2_complete),mean) %>%
  select(-cluster)

ggplot(pivot_longer(as_tibble(cluster_center,  rownames = "cluster"), cols = colnames(cluster_center), names_to = "feature"),
       aes(x = value, y = feature, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))


# Hopkins

get_clust_tendency(income_education_scaled_clean,n=10)
get_clust_tendency(hospital_age_commuter_householder_scaled_clean,n=10)


### show clustering in k
income_education_scaled_hc1_complete <- income_education_scaled %>%
  add_column(cluster = factor(clusters1_complete))


income_education_scaled_hc1_ward <- income_education_scaled %>%
  add_column(cluster = factor(clusters1_ward))


income_education_scaled_hc2_complete <- hospital_age_commuter_householder_scaled %>%
  add_column(cluster = factor(clusters2_complete))


income_education_scaled_hc2_ward <- hospital_age_commuter_householder_scaled %>%
  add_column(cluster = factor(clusters2_ward))


x <- income_education_scaled$median_income
y <-income_education_scaled$poverty_percentage

ggplot(income_education_scaled_hc1_complete, aes(x, y, color = cluster)) + 
  geom_point() 


### k-means

km_income <- kmeans(income_scale_clean, centers = 5, nstart = 10)
km_hospital <- kmeans(hospital_age_commuter_householder_scaled_clean, centers = 4, nstart = 10)

d1 <-dist(income_scale_clean)
d2 <-dist(hospital_scale_clean)

fpc::cluster.stats(d1, km_income$cluster)


# WCSS & average silhouette width
sapply(
  list(
    km_income$cluster
  ),
  FUN = function(x)
    fpc::cluster.stats(d1, x))[c("within.cluster.ss", "avg.silwidth"), ]

sapply(
  list(
    km_hospital$cluster
  ),
  FUN = function(x)
    fpc::cluster.stats(d2, x))[c("within.cluster.ss", "avg.silwidth"), ]

# Silhouette plot
fviz_silhouette(silhouette(km_income$cluster, d1))
fviz_silhouette(silhouette(km_hospital$cluster, d2))

#External vaild
clu<-km_hospital$cluster
d <- d2
#Raw_truth <- read.csv("subset2_with_death_rate.csv")
#read the subset with death_per_cases
#summary(Raw_truth)
Raw_truth <- read.xlsx("subset2_with_death_rate.xlsx",sheetIndex = 1)#choose one
#summary(Raw_truth)
Raw_truth$pam_cluster <- clu#add col with your methods name
#summary(Raw_truth)
write.xlsx(Raw_truth,"subset2_PAM_with_death_rate.xlsx")#write to xlsx and add a truth col

pam_subset2_valid_data <- read.xlsx("subset2_PAM_with_death_rate.xlsx",sheetIndex = 1)#read it
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
    unlist(fpc::cluster.stats(d, clu, truth, compareonly = TRUE)),
    entropy = entropy(clu, truth),
    purity = purity(clu, truth)
  ))
r


