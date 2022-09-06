library(tidyverse)
library(ggplot2)
library(factoextra)
library(fpc)
library(cluster)

setwd("/Users/evelynxu/Documents/workplace_RStudio")
getwd()

# import data
census <- read.csv("COVID-19_cases_plus_census.csv")
age_scaled <- read.csv("age.csv")
multi_feature_scaled <- read.csv("temp_cases_TX_median_income_income_per_capita_median_age_scaled.csv")

# Test
summary(multi_feature_scaled)
age_scaled <- age_scaled %>%
  select(-X)
multi_feature_scaled <- multi_feature_scaled %>%
  select(-X)



# formate data
census <- as_tibble(census)

# Make character factors for analysis
census <- census %>% mutate_if(is.character,factor)
dim(census)

# select data of Texas
census_TX <- census %>% filter(state == "TX")
dim(census_TX)

census
summary(census_TX[,63:78])

# Merge Variables
census_TX_income <- census_TX %>% 
  filter(confirmed_cases > 100) %>% 
  mutate(income_less_14999 = income_less_10000 + income_10000_14999,
         income_15000_24999 = income_15000_19999 + income_20000_24999, 
         income_25000_34999 = income_25000_29999 + income_30000_34999,
         income_35000_44999 = income_35000_39999 + income_40000_44999,
         income_45000_59999 = income_45000_49999 + income_50000_59999, 
         income_60000_99999 = income_60000_74999 + income_75000_99999, 
         income_100000_149999 = income_100000_124999 + income_125000_149999,
         income_150000_or_more = income_150000_199999 + income_200000_or_more)%>%
  select(county_name, confirmed_cases, deaths, total_pop,
         income_less_14999,income_15000_24999,income_25000_34999,
         income_35000_44999,income_45000_59999,income_60000_99999,
         income_100000_149999,income_150000_or_more
         ) %>%
  as_tibble()

# Try not merge
census_TX_income <- census_TX %>% 
  filter(confirmed_cases > 100) %>% 
  mutate(
    cases_per_1000 = confirmed_cases/total_pop*1000,
    deaths_per_1000 = deaths/total_pop*1000,
    deaths_per_case = deaths/confirmed_cases,
    income_less_10000_density = income_less_10000/total_pop,
    income_10000_14999_density = income_10000_14999/total_pop,
    income_15000_19999_density = income_15000_19999/total_pop,
    income_20000_24999_density = income_20000_24999/total_pop,
    income_25000_29999_density = income_25000_29999/total_pop,
    income_30000_34999_density = income_30000_34999/total_pop,
    income_35000_39999_density = income_35000_39999/total_pop,
    income_40000_44999_density = income_40000_44999/total_pop,
    income_45000_49999_density = income_45000_49999/total_pop,
    income_50000_59999_density = income_50000_59999/total_pop,
    income_60000_74999_density = income_60000_74999/total_pop,
    income_75000_99999_density = income_75000_99999/total_pop,
    income_100000_124999_density = income_100000_124999/total_pop,
    income_125000_149999_density = income_125000_149999/total_pop,
    income_150000_199999_density = income_150000_199999/total_pop,
    income_200000_or_more_density = income_200000_or_more/total_pop
  )%>%
  select(county_name, confirmed_cases, deaths, total_pop,
         income_less_10000_density,
         income_10000_14999_density,
         income_15000_19999_density,
         income_20000_24999_density,
         income_25000_29999_density,
         income_30000_34999_density,
         income_35000_39999_density,
         income_40000_44999_density,
         income_45000_49999_density,
         income_50000_59999_density,
         income_60000_74999_density,
         income_75000_99999_density,
         income_100000_124999_density,
         income_125000_149999_density,
         income_150000_199999_density,
         income_200000_or_more_density
  ) %>%
  as_tibble()

census_TX_income
mode(census_TX_income$cases_per_1000)

summary(census_TX_income)
sum(is.na(census_TX_income))

df <- summary(census_TX_income)
write.csv(df,file = "summary.csv")

census_TX_income <- census_TX_income %>%
  mutate(
    cases_per_1000 = confirmed_cases/total_pop*1000,
    deaths_per_1000 = deaths/total_pop*1000,
    deaths_per_case = deaths/confirmed_cases,
    income_less_14999_density = income_less_14999/total_pop,
    income_15000_24999_density = income_15000_24999/total_pop,
    income_25000_34999_density = income_25000_34999/total_pop,
    income_35000_44999_density = income_35000_44999/total_pop,
    income_45000_59999_density = income_45000_59999/total_pop,
    income_60000_99999_density = income_60000_99999/total_pop,
    income_100000_149999_density = income_100000_149999/total_pop,
    income_150000_or_more_density = income_150000_or_more/total_pop
  )

census_TX_income

census_TX_income_mode <- census_TX_income %>%
  select(-county_name)
library(purrr)

FindMode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

map_dbl(census_TX_income_mode, FindMode)

sd(census_TX_income$cases_per_1000)
sd(census_TX_income$deaths_per_1000)
sd(census_TX_income$deaths_per_case)
sd(census_TX_income$income_less_14999_density)
sd(census_TX_income$income_15000_24999_density)
sd(census_TX_income$income_25000_34999_density)
sd(census_TX_income$income_35000_44999_density)
sd(census_TX_income$income_45000_59999_density)
sd(census_TX_income$income_60000_99999_density)
sd(census_TX_income$income_100000_149999_density)
sd(census_TX_income$income_150000_or_more_density)


# income_less_14999_density
# income_15000_24999_density
# income_25000_34999_density
# income_35000_44999_density
# income_45000_59999_density
# income_60000_99999_density
# income_100000_149999_density
# income_150000_or_more_density


income_scaled <- census_TX_income %>%
  select(
    income_less_14999_density,
    income_15000_24999_density,
    income_25000_34999_density,
    income_35000_44999_density,
    income_45000_59999_density,
    income_60000_99999_density,
    income_100000_149999_density,
    income_150000_or_more_density,
  )%>%
  scale()%>%
  as_tibble()

income_scaled <- census_TX_income %>%
  select(
    income_less_10000_density,
    income_10000_14999_density,
    income_15000_19999_density,
    income_20000_24999_density,
    income_25000_29999_density,
    income_30000_34999_density,
    income_35000_39999_density,
    income_40000_44999_density,
    income_45000_49999_density,
    income_50000_59999_density,
    income_60000_74999_density,
    income_75000_99999_density,
    income_100000_124999_density,
    income_125000_149999_density,
    income_150000_199999_density,
    income_200000_or_more_density
  )%>%
  scale()%>%
  as_tibble()

write.csv(income_scaled,file = "subset_income.csv",row.names = FALSE)
df <- summary(income_scaled)
write.csv(df,file = "summary.csv")

library(mclust)

m_clust <- Mclust(as.matrix(income_scaled),G=1:20)
summary(m_clust)
plot(m_clust, "BIC")

pamk.best <- pamk(income_education_scaled)
pamk.best$nc
clusplot(pam(income_education_scaled,pamk.best$nc))

### Elbow method (look at the knee)
fviz_nbclust(income_scaled,hcut,method = "wss") +
  geom_vline(xintercept = 4,linetype = 2)

# Average silhouette
fviz_nbclust(income_scaled, hcut, method = "silhouette")

### Gap statistic
gap_stat <- clusGap(income_scaled, FUN = hcut, K.max = 10, B = 10)
fviz_gap_stat(gap_stat)

fviz_cluster(m_clust,income_scaled)

?fviz_nbclust
?fviz_cluster

d1 <- dist(income_scaled)
d2 <- dist(age_scaled)
d3 <- dist(multi_feature_scaled)

# hierarchical clustering
hc_complete <- hclust(d1,method = "complete")
hc_single <- hclust(d1,method = "single")
hc_ward <- hclust(d1,method = "ward.D")

plot(hc_complete,hang = -1)
plot(hc_single,hang = -1)
plot(hc_ward,hang = -1)
# specify the number of cluster
k <- 4

fviz_dend(hc_complete,k)
clusters_complete <- cutree(hc_complete,k)
# clusters_complete

fviz_dend(hc_single,k)
clusters_single <- cutree(hc_single,k)
# clusters_single

fviz_dend(hc_ward,k)
clusters_ward <- cutree(hc_ward,k)
# clusters_ward

cluster_complete <- census_TX_income%>%
  add_column(cluster = factor(clusters_complete))
cluster_complete <- census_TX_income%>%
  add_column(cluster = factor(clusters_single))
cluster_complete <- census_TX_income%>%
  add_column(cluster = factor(clusters_ward))

cluster_complete <- cluster_complete %>% mutate(county = county_name %>% 
                                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

census_TX_income <- census_TX_income %>% mutate(county = county_name %>% 
                                                  str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', ''))

census_TX_clust <- counties_TX %>% left_join(cluster_complete)
census_TX_clust


ggplot(census_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")


# compare different feature sets
cluster.stats(d1,clusters_complete)

hc_complete <- hclust(d1,method = "complete")
hc_single <- hclust(d1,method = "single")
hc_ward <- hclust(d1,method = "ward.D")

hc_complete <- hclust(d2,method = "complete")
hc_single <- hclust(d2,method = "single")
hc_ward <- hclust(d2,method = "ward.D")

hc_complete <- hclust(d3,method = "complete")
hc_single <- hclust(d3,method = "single")
hc_ward <- hclust(d3,method = "ward.D")

k <- 2
sapply(
  list(
    clusters_complete = cutree(hc_complete,k),
    clusters_single = cutree(hc_single,k),
    clusters_ward = cutree(hc_ward,k)
  ),
  FUN = function(x)
    fpc::cluster.stats(d1, x))[c("within.cluster.ss", "avg.silwidth"), ]

clusters_complete = cutree(hc_complete,k)
clusters_single = cutree(hc_single,k)
clusters_ward = cutree(hc_ward,k)

fviz_silhouette(silhouette(clusters1_complete, d1))
fviz_silhouette(silhouette(clusters_single, d1))
fviz_silhouette(silhouette(clusters_ward, d1))

fviz_silhouette(silhouette(clusters_complete, d2))
fviz_silhouette(silhouette(clusters_single, d2))
fviz_silhouette(silhouette(clusters_ward, d2))

fviz_silhouette(silhouette(clusters_complete, d3))
fviz_silhouette(silhouette(clusters_single, d3))
fviz_silhouette(silhouette(clusters_ward, d3))


# compare different methods
ks <- 4:13
d <- d3
m <- "complete"
# m <- "single"
# m <- "ward.D"

# Within-cluster sum of squares
WCSS <- sapply(ks, FUN = function(k){
  hc<- hclust(d,method = m)
  clusters = cutree(hc,k)
  cluster.stats(d1,clusters)$within.cluster.ss
})
WCSS
ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

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

# Dunn Index
DI <- sapply(ks, FUN=function(k) {
  hc<- hclust(d,method = m)
  clusters = cutree(hc,k)
  cluster.stats(d1,clusters)$dunn
})
best_k <- ks[which.max(DI)]
best_k
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)










# k-means
n <- 6
km_income <- kmeans(income_scaled, centers = n)
km_income$centers

census_TX_clust <- counties_TX %>% left_join(census_TX_income %>% 
                                               add_column(cluster = factor(km_income$cluster)))
ggplot(census_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

ggplot(pivot_longer(as_tibble(km_income$centers,  rownames = "cluster"), 
                    cols = colnames(km_income$centers)), 
       aes(y = name, x = value,fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

ggplot(census_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")


