library("xlsx")
library(scales)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra) 
library("dbscan")


# clean income subset/hospital subset
income_education <- read.csv("income_education.csv", 1)
income_subset <- income_education %>%
  select(poverty_percentage, employed_pop_percentage, bachelors_degree_or_higher_25_64_percentage, median_income, income_per_capita)

hospital_age_commuter_household <- read.csv("hospital_age_commuter_householder.csv", 1)
hospital_subset <- hospital_age_commuter_household %>%
  filter(!is.na(num_hospital_per_1000)) %>%
  select(commuters_by_public_transportation_per_1000, households_percentage, pop_65_to_more_percentage, num_hospital_per_1000)

# scale income subset hospital suset
income_scale <- income_subset %>% scale() %>% as_tibble()
hospital_scale <- hospital_subset %>% scale() %>% as_tibble()

# remove outliers
lof <- lof(income_scale, minPts = 10)
hist(lof, breaks = 10, main = "LOF (minPts = 10)")
ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.2, color = "red", linetype = 2)
income_scale_clean <- income_scale %>%
  add_column(lof = lof) %>%
  filter(lof < 1.2)%>%
  select(-lof)

lof <- lof(hospital_scale, minPts = 10)
hist(lof, breaks = 10, main = "LOF (minPts = 10)")
ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.2, color = "red", linetype = 2)
hospital_scale_clean <- hospital_scale %>%
  add_column(lof = lof) %>%
  filter(lof < 1.2)%>%
  select(-lof)

# determine suitable number of clusters
k <- 2:20

wcss <- sapply(k, FUN = function(k) {kmeans(income_scale_clean, centers = k, nstart = 10)$tot.withinss})
ggplot(as_tibble(k, wcss), aes(k, wcss)) + geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2)

wcss <- sapply(k, FUN = function(k) {kmeans(hospital_scale_clean, centers = k, nstart = 10)$tot.withinss})
ggplot(as_tibble(k, wcss), aes(k, wcss)) + geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2)

# k-means clustering
km_income <- kmeans(income_scale_clean, centers = 5, nstart = 10)
km_hospital <- kmeans(hospital_scale_clean, centers = 4, nstart = 10)

km_income <- kmeans(income_scale_clean, centers = 5)
km_hospital <- kmeans(hospital_scale_clean, centers = 4)
fviz_cluster(km_income, data = income_scale_clean, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

### Plot
ggplot(pivot_longer(as_tibble(km_income$centers,  rownames = "cluster"), 
                    cols = colnames(km_income$centers)),
                    aes(y = name, x = value, fill = cluster)) + 
  geom_bar(stat = "identity") + 
  facet_grid(rows = vars(cluster))
fviz_cluster(km_income, data = income_scale_clean, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

ggplot(pivot_longer(as_tibble(km_hospital$centers,  rownames = "cluster"), 
                    cols = colnames(km_hospital$centers)),
       aes(y = name, x = value, fill = cluster)) + 
  geom_bar(stat = "identity") + 
  facet_grid(rows = vars(cluster))
fviz_cluster(km_hospital, data = hospital_scale_clean, centroids = TRUE, repel = TRUE, ellipse.type = "norm")
