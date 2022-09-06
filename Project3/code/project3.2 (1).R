library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(caret)
library(tidyverse)
library(DT)
library(rpart)
library(rpart.plot)
library(lattice)
library(seriation)
library(FSelector)
library(RWeka)
library(partykit)
library(C50)
library(plyr)
library(dplyr)

## Read and prepare data
setwd("/Users/shuangliang/Documents/SMU/CS7331 Datamining/pro3")
getwd()
census <- read.csv("NewNewNewnew_COVID-19_cases_2021_01_19.csv")

census

cases <- census %>% mutate_if(is.character,factor)
cases <- as_tibble(cases)

dim(cases)
table(complete.cases(cases))


cases <- cases %>% 
  filter(confirmed_cases > 0) %>%
  filter(deaths >= 0) %>%
  filter(delta_deaths >= 0)

dim(cases)
table(complete.cases(cases))


cases <- cases %>%
  arrange(desc(confirmed_cases)) %>%
  mutate(
    cases_per_10000 = confirmed_cases/total_pop*10000,
    deaths_per_10000 = deaths/total_pop*10000,
    death_per_case = deaths/confirmed_cases,
    
    delta_deaths = delta_deaths/total_pop*10000,
    delta_confirmed_cases = delta_confirmed_cases/total_pop*10000
  )

cases <- cases %>% 
  mutate(
    income_less_14999 = income_less_10000 + income_10000_14999,
    income_15000_24999 = income_15000_19999 + income_20000_24999, 
    income_25000_34999 = income_25000_29999 + income_30000_34999,
    income_35000_44999 = income_35000_39999 + income_40000_44999,
    income_45000_59999 = income_45000_49999 + income_50000_59999, 
    income_60000_99999 = income_60000_74999 + income_75000_99999, 
    income_100000_149999 = income_100000_124999 + income_125000_149999,
    income_150000_or_more = income_150000_199999 + income_200000_or_more,
    
    male_65_to_more = male_65_to_66 + male_67_to_69,male_70_to_74 + male_75_to_79 + male_80_to_84 + male_85_and_over,
    female_65_to_more = female_65_to_66 + female_67_to_69 + female_70_to_74 + female_75_to_79 + female_80_to_84 + female_85_and_over
  )

cases

cases_sel <- cases %>% 
  select(
    county_name, state, total_pop,
    nonfamily_households, median_year_structure_built,        
    female_pop, median_age, white_pop, 
    black_pop, asian_pop, hispanic_pop, amerindian_pop,
    commuters_by_public_transportation, 
    households, median_income, housing_units, 
    vacant_housing_units,
    percent_income_spent_on_rent,
    employed_pop, unemployed_pop,
    in_school, in_undergrad_college,
    
    income_per_capita,poverty,
    bachelors_degree_or_higher_25_64,
    
    income_less_14999,income_15000_24999,income_25000_34999,
    income_35000_44999,income_45000_59999,income_60000_99999,
    income_100000_149999,income_150000_or_more,
    
    male_65_to_more,female_65_to_more,
    
    cases_per_10000, deaths_per_10000, death_per_case, delta_deaths, delta_confirmed_cases
  )

cases_sel <- cases_sel %>% mutate(
  nonfamily_households = nonfamily_households / total_pop, 
  female_pop = female_pop / total_pop,
  white_pop = white_pop / total_pop, 
  black_pop = black_pop / total_pop, 
  asian_pop = asian_pop / total_pop, 
  hispanic_pop = hispanic_pop / total_pop, 
  amerindian_pop = amerindian_pop / total_pop,
  commuters_by_public_transportation = commuters_by_public_transportation/ total_pop, 
  households = households / total_pop, 
  housing_units = housing_units / total_pop, 
  vacant_housing_units = vacant_housing_units / total_pop,
  employed_pop = employed_pop / total_pop,
  unemployed_pop = unemployed_pop / total_pop,
  in_school = in_school / total_pop,
  in_undergrad_college = in_undergrad_college / total_pop,
  
  poverty = poverty/total_pop,
  bachelors_degree_or_higher_25_64 = bachelors_degree_or_higher_25_64/total_pop,
  
  income_less_14999 = income_less_14999/total_pop,
  income_15000_24999 = income_15000_24999/total_pop,
  income_25000_34999 = income_25000_34999/total_pop,
  income_35000_44999 = income_35000_44999/total_pop,
  income_45000_59999 = income_45000_59999/total_pop,
  income_60000_99999 = income_60000_99999/total_pop,
  income_100000_149999 = income_100000_149999/total_pop,
  income_150000_or_more = income_150000_or_more/total_pop,
  
  male_65_to_more = male_65_to_more/total_pop,
  female_65_to_more = female_65_to_more/total_pop
)

summary(cases_sel)
table(complete.cases(cases_sel))
str(cases_sel)
cases_sel <- cases_sel %>% na.omit
table(complete.cases(cases_sel))

cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))

ggplot(data = cases_sel, aes(x=delta_deaths))+
  geom_histogram(binwidth = 1,fill = "#69b3a2",color = "#e9ecef",alpha = 0.9)

ggplot(data = cases_sel, aes(x=delta_confirmed_cases))+
  geom_histogram(binwidth = 15,fill = "#69b3a2",color = "#e9ecef",alpha = 0.9)



# cases_sel <- cases_sel %>%
#   mutate(
#     risk = case_when(cases_per_10000 > 900 ~ "high",
#                      cases_per_10000 <= 900 & cases_per_10000 > 700 ~ "medium",
#                      cases_per_10000 <= 700 ~ "low")
#   )

cases_sel <- cases_sel %>%
  mutate(
    risk = case_when(
      delta_confirmed_cases > 29.14 & delta_deaths >= 0.5152  ~ "high",
      delta_confirmed_cases > 29.14 & delta_deaths < 0.5152 ~ "medium",

      delta_confirmed_cases <= 29.14 & delta_deaths >= 0.5152 ~ "medium",
      delta_confirmed_cases <= 29.14 & delta_deaths < 0.5152 ~ "low"
      )
  )

cases_sel <- cases_sel %>%
  mutate(
    risk = case_when(
      delta_confirmed_cases > 29.14 & delta_deaths >= 1.1  ~ "high",
      delta_confirmed_cases > 29.14 & delta_deaths < 1.1 ~ "medium",
      
      delta_confirmed_cases <= 29.14 & delta_deaths >= 1.1 ~ "medium",
      delta_confirmed_cases <= 29.14 & delta_deaths < 1.1 ~ "low"
    )
  )


cases_sel %>% pull(risk) %>% table()

high_state <- cases_sel %>% group_by(state) %>%
  dplyr::summarize(high_pct = sum(risk == "high")/n()) %>%
  arrange(desc(high_pct))

medium_state <- cases_sel %>% group_by(state) %>%
  dplyr::summarize(medium_pct = sum(risk == "medium")/n()) %>%
  arrange(desc(medium_pct))

low_state <- cases_sel %>% group_by(state) %>%
  dplyr::summarize(low_pct = sum(risk == "low")/n()) %>%
  arrange(desc(low_pct))


test_state = c(high_state$state[1:15],medium_state$state[1:15], low_state$state[1:15])

# Select features
# cases_feature <- cases_sel %>% select(-county_name,-state,-delta_confirmed_cases,-delta_deaths,-cases_per_10000,-deaths_per_10000,-death_per_case)
# weights <- cases_feature %>% chi.squared(risk ~ ., data = .) %>%
#   as_tibble(rownames = "feature") %>%
#   arrange(desc(attr_importance))
# 
# weights
# 
# ggplot(weights,
#        aes(x = attr_importance, y = reorder(feature, attr_importance))) +
#   geom_bar(stat = "identity") +
#   xlab("Importance score") + ylab("Feature")
# 
# # feature_imp <- cases_train %>% select(-county_name,-state)
# 
# feature <- cases_feature %>% gain.ratio(risk~., data = .) %>%
#   as_tibble(rownames = "feature") %>%
#   arrange(desc(attr_importance))
# 
# cases_feature %>% cfs(risk ~ ., data = .)
# 
# evaluator <- function(subset) {
#   model <- cases_feature %>% train(as.simple.formula(subset, "risk"),
#                                data = .,
#                                method = "rpart",
#                                trControl = trainControl(method = "boot", number = 5),
#                                tuneLength = 0)
#   results <- model$resample$Accuracy
#   cat("Trying features:", paste(subset, collapse = " + "), "\n")
#   m <- mean(results)
#   cat("Accuracy:", round(m, 2), "\n\n")
#   m
# }
# 
# features <- cases_feature %>% colnames() %>% setdiff("risk")
# subset <- backward.search(features, evaluator)
# subset <- forward.search(features, evaluator)
# subset <- best.first.search(features, evaluator)
# subset <- hill.climbing.search(features, evaluator)
# subset

# Prepare data
cases_sel <- cases_sel %>%
  select(
    county_name, state, risk,
    nonfamily_households,
    female_pop,
    median_age,
    white_pop,
    black_pop,
    hispanic_pop,
    amerindian_pop,
    employed_pop,
    
    # cases_per_10000, deaths_per_10000, death_per_case, delta_deaths, delta_confirmed_cases
  )


cases_sel <- cases_sel %>% mutate_if(is.character,factor)
summary(cases_sel)
table(complete.cases(cases_sel))
str(cases_sel)
cases_sel <- cases_sel %>% na.omit
table(complete.cases(cases_sel))

cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))

# Test data selected by the rank of states
cases_train <- cases_sel %>% filter(state %in% test_state)
cases_train %>% pull(risk) %>% table()

cases_test <- cases_sel %>% filter(!(state %in% test_state))
cases_test %>% pull(risk) %>% table()

# Training data 80%
inTrain <- createDataPartition(y = cases_sel$risk, p = .8, list = FALSE)
cases_train <- cases_sel %>% slice(inTrain)
cases_test <- cases_sel %>% slice(-inTrain)

cases_train %>% pull(risk) %>% table()
cases_test %>% pull(risk) %>% table()

# Plot Map
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  dplyr::rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties 

counties_all <- counties %>% 
  left_join(cases_train %>% 
              mutate(county = county_name %>% 
                       str_to_lower() %>%
                       str_replace('\\s+county\\s*$', '')))
dev.off()
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))


## Train

# ctree
train_index <- createFolds(cases_train$risk, k = 10)

ctreeFit <- cases_train %>% train(risk ~ . - county_name - state,
                                  method = "ctree",
                                  data = .,
                                  tuneLength = 10,
                                  trControl = trainControl(method = "cv", indexOut = train_index))
ctreeFit
ctreeFit$finalModel

# C4.5
C45Fit <- cases_train %>% train(risk ~ . - county_name - state,
                                method = "J48",
                                data = .,
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", indexOut = train_index))
C45Fit

C45Fit$finalModel


# knn
knnFit <- cases_train %>% train(risk ~ . - county_name - state,
                                method = "knn",
                                data = .,
                                preProcess = "scale",
                                tuneLength = 5,
                                tuneGrid=data.frame(k = 1:20),
                                trControl = trainControl(method = "cv", indexOut = train_index))
knnFit_choosed <- cases_train %>% train(risk ~ . - county_name - state,
                                method = "knn",
                                data = .,
                                preProcess = "scale",
                                tuneLength = 5,
                                tuneGrid=data.frame(k = 3),
                                trControl = trainControl(method = "cv", indexOut = train_index))
knnFit
knnFit_choosed

kknnFit <- cases_train %>% train(risk ~ . - county_name - state,
                                method = "kknn",
                                data = .,
                                preProcess = "scale",
                                tuneLength = 10,
                                # tuneGrid=data.frame(k = 1:20),
                                trControl = trainControl(method = "cv", indexOut = train_index))
kknnFit

# model <- x %>% knn3(Species ~ ., data = ., k = 1)
# decisionplot(knnFit, cases_train, class_var = "risk") + labs(title = "kNN (1 neighbor)")


# C5.0
# trControl <- trainControl(method = "cv", number = 5, selectionFunction = "oneSE")
# 
# grid <- expand.grid(
#   .model = 'tree',
#   .trials = c(1,3,5),
#   .winnow = 'FALSE'
# )
# 
# c50Fit <- cases_train %>% 
#   train(risk ~ . - county_name - state,
#         method = 'C5.0',
#         data = .,
#         trControl = trControl,
#         tuneGrid = grid
#   )

c50Fit <- cases_train %>% 
  train(risk ~ . - county_name - state,
        method = 'C5.0',
        data = .,
        tuneLength = 20,
        trControl = trainControl(method = "cv", indexOut = train_index)
  )

c50Fit

# SVM
svmFit <- cases_train %>% train(risk ~ . - county_name - state,
                                method = "svmLinear",
                                data = .,
                                tuneLength = 15,
                                trControl = trainControl(method = "cv", indexOut = train_index))
svmFit
svmFit$finalModel

# xgboost
xgboostFit <- cases_train %>% train(risk ~ . - county_name - state,
                                    method = "xgbTree",
                                    data = .,
                                    tuneLength = 5,
                                    trControl = trainControl(method = "cv", indexOut = train_index),
                                    tuneGrid = expand.grid(
                                      nrounds = 20,
                                      max_depth = 3,
                                      colsample_bytree = .6,
                                      eta = 0.1,
                                      gamma=0,
                                      min_child_weight = 1,
                                      subsample = .5
                                    ))
xgboostFit
xgboostFit$finalModel
# ANN
nnetFit <- cases_train %>% train(risk ~ . - county_name - state,
                                 method = "nnet",
                                 data = .,
                                 tuneLength = 5,
                                 trControl = trainControl(method = "cv", indexOut = train_index),
                                 trace = FALSE)
nnetFit
nnetFit$finalModel



















## Comparing Models
resamps <- resamples(list(
  #ctree = ctreeFit,
  #C45 = C45Fit,
  #KNN = knnFit,
  #KNN_3 = knnFit_choosed,
  #KKNN = kknnFit,
  #C50 = c50Fit,
  SVM = svmFit,
  xgboost = xgboostFit,
  NeuralNet = nnetFit
))

resamps
summary(resamps)
bwplot(resamps,layout = c(3,1))
difs <- diff(resamps)
difs
summary(difs)

## Test Data
# cTree
cases_test$risk_predicted <- predict(ctreeFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)


# C4.5
cases_test$risk_predicted <- predict(C45Fit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)

# KNN
cases_test$risk_predicted <- predict(knnFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)

# KNN_choosed(k=3)
cases_test$risk_predicted <- predict(knnFit_choosed, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)

# KKNN
cases_test$risk_predicted <- predict(kknnFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)


# C5.0
cases_test$risk_predicted <- predict(c50Fit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)


# SVM
cases_test$risk_predicted <- predict(svmFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)


# xgBoost
cases_test$risk_predicted <- predict(xgboostFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)


# NN
cases_test$risk_predicted <- predict(nnetFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)















