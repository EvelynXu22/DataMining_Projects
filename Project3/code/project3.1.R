setwd("/Users/evelynxu/Documents/workplace_RStudio")
getwd()

## Read and prepare data
census <- read.csv("COVID-19_cases_plus_census.csv")

census

cases <- census %>% mutate_if(is.character,factor)
cases <- as_tibble(cases)
dim(cases)

cases <- cases %>% filter(confirmed_cases > 0)
cases <- cases %>%
  arrange(desc(confirmed_cases)) %>%
  mutate(
    cases_per_10000 = confirmed_cases/total_pop*10000,
    deaths_per_10000 = deaths/total_pop*10000,
    death_per_case = deaths/confirmed_cases
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
    
    cases_per_10000, deaths_per_10000, death_per_case
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

cases_sel

ggplot(data = cases_sel, aes(x=cases_per_10000))+
  geom_histogram(binwidth = 15,fill = "#69b3a2",color = "#e9ecef",alpha = 0.9)

summary(cases_sel)
table(complete.cases(cases_sel))
str(cases_sel)
cases_sel <- cases_sel %>% na.omit
table(complete.cases(cases_sel))

cm <- cor(cases_sel %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))

## Create class variable
cases_sel <- cases_sel %>%
  mutate(
    risk = case_when(cases_per_10000 > 1100 ~ "high",
                     cases_per_10000 <= 1100 & cases_per_10000 > 500 ~ "medium",
                     cases_per_10000 <= 500 ~ "low")
  )
cases_sel %>% pull(risk) %>% table()

cases_sel %>% group_by(state) %>%
  summarize(high_pct = sum(risk == "high")/n()) %>%
  arrange(desc(high_pct))

cases_sel %>% group_by(state) %>%
  summarize(low_pct = sum(risk == "low")/n()) %>%
  arrange(desc(low_pct))

## Split into training and test data
cases_train <- cases_sel %>%
  filter(state %in% c("TX","CA","FL","NY","ME"))
cases_train %>% pull(risk) %>% table()

cases_test <- cases_sel %>% filter(!(state %in% c("TX","CA","FL","NY","ME")))
cases_test %>% pull(risk) %>% table()

## Select Features
# plot a map for test data
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
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

# check variable importance
cases_train <- cases_train %>% 
  select(-deaths_per_10000,-death_per_case,-cases_per_10000)

cases_train %>% chi.squared(risk ~ .,data = .) %>%
  arrange(desc(attr_importance)) %>%
  head(n = 10)

## build a model
fit <- cases_train %>%
  train(risk ~ . - county_name - state,
        data = .,
        method = "rf",
        trControl = trainControl(method = "cv", number = 10)
  )
fit

varImp(fit)

# cases_test <- cases_test %>% na.omit
cases_test$risk_predicted <- predict(fit, cases_test)

counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = risk_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))

cases_test <- cases_test %>% mutate_if(is.character,factor)
confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)
