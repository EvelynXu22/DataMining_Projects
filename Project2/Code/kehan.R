library("xlsx")
library(scales)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra) 
library("dbscan")

getwd()
setwd("/Users/kehanzhang/Desktop/R")




### K-means clustering
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
lof1 <- lof(income_scale, minPts = 10)
hist(lof1, breaks = 10, main = "LOF (minPts = 10)")
ggplot(tibble(index = seq_len(length(lof1)), lof = sort(lof1)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.4, color = "red", linetype = 2)
income_scale_clean <- income_scale %>%
  add_column(lof = lof1) %>%
  filter(lof < 1.4) %>%
  select(poverty_percentage, employed_pop_percentage, bachelors_degree_or_higher_25_64_percentage, median_income, income_per_capita)

lof2 <- lof(hospital_scale, minPts = 10)
hist(lof2, breaks = 10, main = "LOF (minPts = 10)")
ggplot(tibble(index = seq_len(length(lof2)), lof = sort(lof2)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.5, color = "red", linetype = 2)
hospital_scale_clean <- hospital_scale %>%
  add_column(lof = lof2) %>%
  filter(lof < 1.5) %>%
  select(commuters_by_public_transportation_per_1000, households_percentage, pop_65_to_more_percentage, num_hospital_per_1000)

# determine suitable number of clusters
k <- 2:20

wcss <- sapply(k, FUN = function(k) {kmeans(income_scale_clean, centers = k, nstart = 10)$tot.withinss})
ggplot(as_tibble(k, wcss), aes(k, wcss)) + geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2)

wcss <- sapply(k, FUN = function(k) {kmeans(hospital_scale_clean, centers = k, nstart = 10)$tot.withinss})
ggplot(as_tibble(k, wcss), aes(k, wcss)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

fviz_nbclust(income_scale_clean,kmeans,method = "silhouette")
fviz_nbclust(hospital_scale_clean,kmeans,method = "silhouette")

# k-means clustering
km_income <- kmeans(income_scale_clean, centers = 5, nstart = 10)
km_hospital <- kmeans(hospital_scale_clean, centers = 4, nstart = 10)

# cluster profile of subset income
ggplot(pivot_longer(as_tibble(km_income$centers,  rownames = "cluster"), 
                    cols = colnames(km_income$centers)),
                    aes(y = name, x = value, fill = cluster)) + 
  geom_bar(stat = "identity") + 
  facet_grid(rows = vars(cluster))
fviz_cluster(km_income, data = income_scale_clean, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

# cluster profile of subset hospital
ggplot(pivot_longer(as_tibble(km_hospital$centers,  rownames = "cluster"), 
                    cols = colnames(km_hospital$centers)),
       aes(y = name, x = value, fill = cluster)) + 
  geom_bar(stat = "identity") + 
  facet_grid(rows = vars(cluster))
fviz_cluster(km_hospital, data = hospital_scale_clean, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

# show infection rate and death rate on map
counties <- as_tibble(map_data("county"))
counties_tx <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

infection_death_map <- income_education %>%
  mutate(county = county_name %>% 
         str_to_lower() %>% 
         str_replace('\\s+county\\s*$', ''))
infection_death_map <- counties_tx %>% left_join(infection_death_map)

ggplot(infection_death_map, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() +
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Cases per 1000 People of Counties in Texas", subtitle = "Only counties reporting 100+ cases")

ggplot(infection_death_map, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Deaths per 1000 People of Counties in Texas", subtitle = "Only counties reporting 100+ cases")

ggplot(infection_death_map, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_case)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Deaths per Case of Counties in Texas", subtitle = "Only counties reporting 100+ cases")

# income subset
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

income_subset_map <- income_education %>%
  select(county_name, poverty_percentage, employed_pop_percentage, bachelors_degree_or_higher_25_64_percentage, median_income, income_per_capita) %>%
  scale_numeric() %>%
  as_tibble()

income_subset_map_clean <- income_subset_map %>%
  add_column(lof = lof1) %>%
  filter(lof < 1.4) 

income_subset_map_clean_5 <- income_subset_map_clean %>%
  select(poverty_percentage, employed_pop_percentage, bachelors_degree_or_higher_25_64_percentage, median_income, income_per_capita)

km_income_map <- kmeans(income_subset_map_clean_5, centers = 5, nstart = 10)

income_subset_map_clean <- income_subset_map_clean %>%
  left_join(income_education %>% select(c(county_name, cases_per_1000, deaths_per_1000, deaths_per_case))) %>%
  mutate(county = county_name %>% 
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', ''))
  
income_cluster_map <- counties_tx %>% 
  left_join(income_subset_map_clean %>% add_column(cluster = factor(km_income_map$cluster)))

ggplot(income_cluster_map, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Subset One Clusters Map", subtitle = "Only counties reporting 100+ cases")

income_cluster_map %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_death_rate = mean(deaths_per_case))


# hospital subset
hospital_subset_map <- hospital_age_commuter_household %>%
  filter(!is.na(num_hospital_per_1000)) %>%
  select(county_name, commuters_by_public_transportation_per_1000, households_percentage, pop_65_to_more_percentage, num_hospital_per_1000) %>%
  scale_numeric() %>%
  as_tibble()

hospital_subset_map_clean <- hospital_subset_map %>%
  add_column(lof = lof2) %>%
  filter(lof < 1.5) 

hospital_subset_map_clean_4 <- hospital_subset_map_clean %>%
  select(commuters_by_public_transportation_per_1000, households_percentage, pop_65_to_more_percentage, num_hospital_per_1000)

km_hospital_map <- kmeans(hospital_subset_map_clean_4, centers = 4, nstart = 10)

hospital_subset_map_clean <- hospital_subset_map_clean %>%
  left_join(hospital_age_commuter_household %>% select(c(county_name, cases_per_1000, deaths_per_1000, deaths_per_case))) %>%
  mutate(county = county_name %>% 
           str_to_lower() %>% 
           str_replace('\\s+county\\s*$', ''))

hospital_cluster_map <- counties_tx %>% 
  left_join(hospital_subset_map_clean %>% add_column(cluster = factor(km_hospital$cluster)))

ggplot(hospital_cluster_map, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Subset Two Clusters Map", subtitle = "Only counties reporting 100+ cases")

hospital_cluster_map %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_death_rate = mean(deaths_per_case))

write.csv(hospital_subset_map_clean,"subset2_with_death_rate.csv")





### Create original dataset
counties_info <- read.csv("COVID-19_cases_plus_census.csv", 1)
hospital <- read.csv("hospital.csv", 1)

# get number of hospital in each county
selected_hospital <- hospital %>%
  group_by(county_name) %>%
  mutate(count = 1) %>%
  summarise(num_hospital = sum(count)) %>%
  mutate(county_name = county_name %>% str_to_lower())

# final raw table
counties_info <- counties_info %>%
  mutate(county_name = county_name %>%
           str_to_lower() %>%
           str_replace('\\s+county\\s*$', '')) %>%
  left_join(selected_hospital)

# final table
counties_info <- counties_info %>%
  mutate(hospital_per_1000_people = num_hospital / total_pop * 1000)

write.csv(counties_info, file = "counties_information.csv")




### Statistic 
all_features <- read.csv("all_features.csv", 1)
income_education <- read.csv("income_education.csv", 1)
hospital_age_commuter_household <- read.csv("hospital_age_commuter_householder.csv", 1)

summary(all_features)
df <- data.frame(all_features[,2:24])
for (i in seq_len(23)) {
  st <- sd(df[,i]);
  print(st);
}

summary(income_education)
df <- data.frame(income_education[,2:7])
for (i in seq_len(6)) {
  st <- sd(df[,i]);
  print(st);
}

summary(hospital_age_commuter_household)
df <- data.frame(hospital_age_commuter_household[,5:8])
for (i in seq_len(23)) {
  st <- sd(df[,i]);
  print(st);
}
